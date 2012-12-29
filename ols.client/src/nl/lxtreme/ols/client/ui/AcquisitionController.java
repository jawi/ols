/*
 * OpenBench LogicSniffer / SUMP project 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
 *
 * Copyright (C) 2006-2010 Michael Poppitz, www.sump.org
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui;


import static nl.lxtreme.ols.client.ui.signaldisplay.view.UIManagerKeys.*;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.ioutil.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.event.Event;
import org.osgi.service.log.*;


/**
 * Provides a front-end controller for all acquisition related tasks.
 */
public final class AcquisitionController
{
  // INNER TYPES

  static interface CancellableTask<T> extends Callable<T>
  {
    // METHODS

    /**
     * Cancels this task.
     */
    void cancel();
  }

  // CONSTANTS

  /** The topic prefix */
  public static final String TOPIC_PREFIX = AcquisitionController.class.getPackage().getName().replaceAll( "\\.", "/" );

  /** The topic used to report progress of the acquisition data. */
  public static final String TOPIC_ACQUISITION_PROGRESS = TOPIC_PREFIX + "/progress";

  public static final String KEY_PROGRESS = "progress";

  /** The topic used to report the status of the acquisition. */
  public static final String TOPIC_ACQUISITION_STATUS = TOPIC_PREFIX + "/status";

  /** All topics. */
  public static final String TOPIC_ANY = TOPIC_PREFIX + "/*";

  /**
   * Provides information about the status of the acquisition. The value is of
   * type String and equals to one of the <tt>STATUS_*</tt> values defined in
   * this same interface.
   */
  public static final String KEY_STATUS = "status";
  /**
   * Provides information about failures during an acquisition. The value is of
   * type {@link Exception}.
   */
  public static final String KEY_EXCEPTION = "exception";
  /**
   * Provides information about the device which is acquiring. The value is of
   * type String and equals to the name of the device.
   */
  public static final String KEY_DEVICE = "device";
  /**
   * Provides information about when the acquisition was started. The value is
   * of type Long and denotes the time in milliseconds (Epoch time).
   */
  public static final String KEY_START_TIME = "startTime";

  public static final String STATUS_STARTED = "started";
  public static final String STATUS_FAILED = "failed";
  public static final String STATUS_CANCELLED = "aborted";
  public static final String STATUS_SUCCESS = "success";

  private static final int MAX_POOL_SIZE = 16;

  // VARIABLES

  // Injected by Felix DM...
  private volatile StatusListener statusListener;
  private volatile EventAdmin eventAdmin;
  private volatile Session session;
  private volatile LogService log;

  private final ConcurrentMap<String, Future<AcquisitionData>> acquisitions;
  private final ExecutorService executorService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquisitionController} instance.
   */
  public AcquisitionController()
  {
    this.acquisitions = new ConcurrentHashMap<String, Future<AcquisitionData>>();
    this.executorService = new ThreadPoolExecutor( 0, MAX_POOL_SIZE, 60L, TimeUnit.SECONDS,
        new SynchronousQueue<Runnable>() )
    {
      @Override
      protected <T> RunnableFuture<T> newTaskFor( final Callable<T> callable )
      {
        return new FutureTask<T>( callable )
        {
          @Override
          @SuppressWarnings( "finally" )
          public boolean cancel( final boolean mayInterruptIfRunning )
          {
            try
            {
              if ( callable instanceof CancellableTask<?> )
              {
                ( ( CancellableTask<?> )callable ).cancel();
              }
            }
            finally
            {
              return super.cancel( mayInterruptIfRunning );
            }
          }
        };
      }
    };
  }

  // METHODS

  /**
   * Cancels any ongoing acquisition for the current selected device.
   */
  public void cancelCapture()
  {
    final AcquisitionDevice device = getCurrentSelectedDevice();
    if ( device == null )
    {
      this.log.log( LogService.LOG_WARNING, "No device is selected, cannot cancel data acquisition!" );
      return;
    }

    try
    {
      this.log.log( LogService.LOG_INFO, "Cancelling data acquisition for '" + device.getName() + "' ..." );

      cancelAcquisition( device );
    }
    catch ( IllegalStateException exception )
    {
      this.log.log( LogService.LOG_WARNING, "Failed to cancel data acquisition; no acquisition in progress!" );
      this.statusListener.setStatus( "No acquisition in progress!" );
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to cancel data acquisition; I/O problem!", exception );
        this.statusListener.setStatus( "I/O problem: " + exception.getMessage() );
      }
    }
  }

  /**
   * Starts an acquisition for the current selected device.
   * 
   * @param aParent
   *          the parent window to use for displaying any other dialog, can be
   *          <code>null</code>.
   */
  public void captureData( final Window aParent )
  {
    final AcquisitionDevice device = getCurrentSelectedDevice();
    if ( device == null )
    {
      this.log.log( LogService.LOG_WARNING, "No device is selected, cannot start data acquisition!" );
      return;
    }

    device.configure( aParent, new ConfigurationListener()
    {
      final LogService log = AcquisitionController.this.log;

      @Override
      public void configurationEvent( final ConfigurationEvent aEvent )
      {
        try
        {
          this.log.log( LogService.LOG_INFO, "Starting data acquisition for '" + device.getName() + "' ..." );

          setStatus( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", //
              device.getName(), new Date() );

          acquireData( device );
        }
        catch ( IOException exception )
        {
          // Make sure to handle IO-interrupted exceptions properly!
          if ( !IOUtil.handleInterruptedException( exception ) )
          {
            this.log.log( LogService.LOG_WARNING, "Failed to start data acquisition; I/O problem!", exception );
            setStatus( "I/O problem: " + exception.getMessage() );
          }
        }
      }
    } );
  }

  /**
   * @return <code>true</code> if there an acquisition ongoing,
   *         <code>false</code> otherwise.
   */
  public boolean isDeviceCapturing()
  {
    AcquisitionDevice device = getCurrentSelectedDevice();
    if ( device == null )
    {
      return false;
    }

    return isAcquisitionInProgress( device );
  }

  /**
   * Returns whether or not a device is selected.
   * 
   * @return <code>true</code> if a device is selected, <code>false</code>
   *         otherwise.
   */
  public boolean isDeviceSelected()
  {
    return ( getCurrentSelectedDevice() != null );
  }

  /**
   * {@inheritDoc}
   */
  public boolean isDeviceSetup()
  {
    final AcquisitionDevice device = getCurrentSelectedDevice();
    return ( device != null ) && device.isSetup();
  }

  /**
   * Restarts a new acquisition with the current device and with its current
   * settings.
   */
  public void repeatCaptureData()
  {
    AcquisitionDevice device = getCurrentSelectedDevice();
    if ( device == null )
    {
      this.log.log( LogService.LOG_WARNING, "No device is selected, cannot repeat data acquisition!" );
      return;
    }
    if ( !device.isSetup() )
    {
      this.log.log( LogService.LOG_WARNING, "Device is not set up, cannot repeat data acquisition!" );
      return;
    }

    try
    {
      this.log.log( LogService.LOG_INFO, "Repeating data acquisition for '" + device.getName() + "' ..." );

      this.statusListener.setStatus( "Capture from {0} started at {1,date,medium} {1,time,medium} ...", //
          device.getName(), new Date() );

      if ( !UIManager.getBoolean( RETAIN_ANNOTATIONS_WITH_RECAPTURE ) )
      {
        getAnnotationData().clearAll();
      }

      acquireData( device );
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to repeat data acquisition; I/O problem!", exception );
        this.statusListener.setStatus( "I/O problem: " + exception.getMessage() );
      }
    }
  }

  /**
   * Called by Felix DM when this component is stopped.
   */
  public void stop() throws InterruptedException
  {
    for ( String deviceName : this.acquisitions.keySet() )
    {
      cancelAcquisition( deviceName );
    }
  }

  /**
   * 
   */
  final void fireAcquisitionCancelledEvent( final String aDeviceName, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( KEY_STATUS, STATUS_CANCELLED );
    props.put( KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_STATUS, props ) );

    this.log.log( LogService.LOG_WARNING, "Acquisition cancelled for " + aDeviceName );
  }

  /**
   * @param aData
   */
  final void fireAcquisitionEndedEvent( final String aDeviceName, final AcquisitionData aData, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( KEY_STATUS, STATUS_SUCCESS );
    props.put( KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_STATUS, props ) );

    this.session.setAcquisitionData( aData );

    this.log.log( LogService.LOG_INFO, "Acquisition successful for " + aDeviceName );
  }

  /**
   * @param aException
   */
  final void fireAcquisitionFailedEvent( final String aDeviceName, final Throwable aException, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( KEY_STATUS, STATUS_FAILED );
    props.put( KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( KEY_EXCEPTION, aException );
    props.put( KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_STATUS, props ) );

    this.log.log( LogService.LOG_WARNING, "Acquisition failed for " + aDeviceName, aException );
  }

  /**
   * @param aPercentage
   */
  final void fireAcquisitionProgressEvent( final String aDeviceName, final int aPercentage )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( KEY_PROGRESS, Integer.valueOf( aPercentage ) );
    props.put( KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_PROGRESS, props ) );
  }

  /**
   * Fires an event to notify listers that an acquisition is started.
   */
  final void fireAcquisitionStartedEvent( final String aDeviceName, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( KEY_STATUS, STATUS_STARTED );
    props.put( KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_STATUS, props ) );

    this.log.log( LogService.LOG_WARNING, "Acquisition started for " + aDeviceName );
  }

  /**
   * @param aMessage
   * @param aArguments
   */
  final void setStatus( final String aMessage, final Object... aArguments )
  {
    this.statusListener.setStatus( aMessage, aArguments );
  }

  /**
   * Acquires data from the given device in the background.
   * 
   * @param aDevice
   *          the device to start an acquisition for, cannot be
   *          <code>null</code>.
   */
  protected void acquireData( final AcquisitionDevice aDevice ) throws IOException
  {
    final String name = aDevice.getName();
    if ( this.acquisitions.containsKey( name ) )
    {
      throw new IllegalStateException( "Acquisition for " + name + " already in progress!" );
    }

    // Wrap the actual acquisition task in order to get a kind of "auto"
    // closable behavior...
    Callable<AcquisitionData> task = new CancellableTask<AcquisitionData>()
    {
      private final long startTime = System.currentTimeMillis();

      @Override
      public AcquisitionData call() throws Exception
      {
        final String name = aDevice.getName();

        fireAcquisitionStartedEvent( name, this.startTime );

        try
        {
          final AcquisitionData result = aDevice.acquireData( new DeviceProgressListener()
          {
            @Override
            public void acquisitionInProgress( final int aPercentage )
            {
              fireAcquisitionProgressEvent( name, aPercentage );
            }
          } );

          fireAcquisitionEndedEvent( name, result, this.startTime );
          updateAdministration( name );

          return result;
        }
        catch ( Exception exception )
        {
          fireAcquisitionFailedEvent( name, exception, this.startTime );
          updateAdministration( name );

          throw exception;
        }
      }

      @Override
      public void cancel()
      {
        fireAcquisitionCancelledEvent( aDevice.getName(), this.startTime );
        updateAdministration( name );

        aDevice.cancelAcquisition();
      }
    };

    this.acquisitions.putIfAbsent( name, this.executorService.submit( task ) );
  }

  /**
   * Cancels the acquisition of the given device.
   * 
   * @param aDevice
   *          the device to cancel the acquisition for, cannot be
   *          <code>null</code>.
   */
  protected void cancelAcquisition( final AcquisitionDevice aDevice ) throws IOException
  {
    final String name = aDevice.getName();

    Callable<Void> cancelTask = new Callable<Void>()
    {
      @Override
      public Void call() throws Exception
      {
        cancelAcquisition( name );
        return null;
      }
    };

    this.executorService.submit( cancelTask );
  }

  /**
   * Returns whether or not there is an acquisition in progress for the given
   * device.
   * 
   * @param aDevice
   *          the device to test, cannot be <code>null</code>.
   * @return <code>true</code> if there is an acquisition in progress for the
   *         given device, <code>false</code> otherwise.
   */
  protected boolean isAcquisitionInProgress( final AcquisitionDevice aDevice )
  {
    final String name = aDevice.getName();
    final Future<AcquisitionData> future = this.acquisitions.get( name );
    return ( future != null ) && !future.isDone();
  }

  /**
   * Cancels the potentially pending acquisition of the device with the given
   * name.
   * 
   * @param aDeviceName
   *          the name of the device to cancel the acquisition for, cannot be
   *          <code>null</code>.
   */
  private void cancelAcquisition( final String aDeviceName )
  {
    Future<AcquisitionData> future = this.acquisitions.remove( aDeviceName );
    if ( future != null )
    {
      if ( !future.cancel( true /* mayInterruptIfRunning */) )
      {
        this.log.log( LogService.LOG_WARNING, "Failed to cancel acquisition?!" );
      }
    }
  }

  /**
   * @return the current annotation data, never <code>null</code>.
   */
  private AnnotationData getAnnotationData()
  {
    return Client.getInstance().getSession().getAnnotationData();
  }

  /**
   * @return the current selected device, can be <code>null</code> if no device
   *         is selected.
   */
  private AcquisitionDevice getCurrentSelectedDevice()
  {
    return getDeviceController().getSelectedDevice();
  }

  /**
   * @return the current device controller, never <code>null</code>.
   */
  private DeviceController getDeviceController()
  {
    return Client.getInstance().getDeviceController();
  }

  /**
   * Updates our administration by removing the future associated to the given
   * device name.
   * 
   * @param aDeviceName
   *          the name of the device whose future should be removed from our
   *          administration.
   */
  private void updateAdministration( final String aDeviceName )
  {
    this.acquisitions.remove( aDeviceName );
  }
}
