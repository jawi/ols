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
package nl.lxtreme.ols.client.acquisition.impl;


import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.client.acquisition.*;
import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Provides a front end for acquiring data.
 */
public class DataAcquirerImpl implements IDataAcquirer
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

  private static final int MAX_POOL_SIZE = 16;

  // VARIABLES

  // Injected by Felix DM...
  private volatile LogService logService;
  private volatile EventAdmin eventAdmin;
  private volatile Session session;

  private final ConcurrentMap<String, Future<AcquisitionData>> acquisitions;
  private final ExecutorService executorService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DataAcquirerImpl} instance.
   */
  public DataAcquirerImpl()
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
   * {@inheritDoc}
   */
  @Override
  public void acquireData( final DeviceInvoker aDevice ) throws IOException
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
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition( final DeviceInvoker aDevice ) throws IOException
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
   * {@inheritDoc}
   */
  @Override
  public boolean isAcquisitionInProgress( final DeviceInvoker aDevice )
  {
    final String name = aDevice.getName();
    final Future<AcquisitionData> future = this.acquisitions.get( name );
    return ( future != null ) && !future.isDone();
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

    this.logService.log( LogService.LOG_WARNING, "Acquisition cancelled for " + aDeviceName );
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

    this.logService.log( LogService.LOG_INFO, "Acquisition successful for " + aDeviceName );
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

    this.logService.log( LogService.LOG_WARNING, "Acquisition failed for " + aDeviceName, aException );
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

    this.logService.log( LogService.LOG_WARNING, "Acquisition started for " + aDeviceName );
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
        this.logService.log( LogService.LOG_WARNING, "Failed to cancel acquisition?!" );
      }
    }
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
