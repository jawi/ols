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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.acquisition.impl;


import static nl.lxtreme.ols.acquisition.AcquisitionConstants.*;
import static nl.lxtreme.ols.task.execution.TaskConstants.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Provides a {@link AcquisitionService} that performs the acquisition in the
 * background.
 */
public class BackgroundDataAcquisitionService implements AcquisitionService, EventHandler
{
  // VARIABLES

  private final ConcurrentMap<String, Device> devices;
  private final Map<String, DeviceConfiguration> deviceConfigs;

  private volatile Future<?> acquisitionFutureTask;
  private volatile EventAdmin eventAdmin;
  private volatile LogService logService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BackgroundDataAcquisitionService} instance.
   */
  public BackgroundDataAcquisitionService()
  {
    this.deviceConfigs = new HashMap<String, DeviceConfiguration>();
    this.devices = new ConcurrentHashMap<String, Device>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( DeviceConfiguration aConfig, final String aDeviceName ) throws IOException
  {
    if ( aConfig == null )
    {
      throw new IllegalArgumentException( "Config cannot be null!" );
    }
    if ( isAcquiring( aDeviceName ) )
    {
      throw new IllegalStateException( "Acquisition still in progress!" );
    }

    Device device = this.devices.get( aDeviceName );
    if ( device == null )
    {
      throw new IllegalArgumentException( "Invalid device name!" );
    }

    // Keep this configuration for later use...
    this.deviceConfigs.put( aDeviceName, aConfig );

    this.acquisitionFutureTask = device.acquireData( aConfig, new AcquisitionProgressListener()
    {
      private int oldPercentage = -1;

      @Override
      public void acquisitionInProgress( final int aPercentage )
      {
        if ( this.oldPercentage != aPercentage )
        {
          fireAcquisitionInProgressEvent( aDeviceName, aPercentage );

          this.oldPercentage = aPercentage;
        }
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( String aDeviceName ) throws IOException
  {
    if ( aDeviceName == null || "".equals( aDeviceName.trim() ) || !this.devices.containsKey( aDeviceName ) )
    {
      throw new IllegalArgumentException( "Invalid device name!" );
    }

    DeviceConfiguration config = this.deviceConfigs.get( aDeviceName );
    if ( config == null )
    {
      throw new IllegalStateException( "No configuration present for " + aDeviceName );
    }

    acquireData( config, aDeviceName );
  }

  /**
   * Adds a given device to this service.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to add, cannot be <code>null</code>.
   */
  public void addDevice( Device aDevice )
  {
    this.devices.putIfAbsent( aDevice.getName(), aDevice );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition( String aDeviceName ) throws IOException, IllegalStateException
  {
    if ( this.acquisitionFutureTask == null )
    {
      throw new IllegalStateException( "No acquisition in progress!" );
    }

    this.acquisitionFutureTask.cancel( true /* mayInterruptIfRunning */);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceConfiguration configureDevice( java.awt.Window aParent, String aDeviceName )
  {
    Device device = this.devices.get( aDeviceName );
    if ( device == null )
    {
      throw new IllegalArgumentException( "Invalid device name!" );
    }

    return device.setupDevice();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceState getState( String aDeviceName )
  {
    if ( this.acquisitionFutureTask != null )
    {
      if ( this.acquisitionFutureTask.isCancelled() )
      {
        return DeviceState.CANCELLED;
      }
      else if ( !this.acquisitionFutureTask.isDone() )
      {
        return DeviceState.ACQUIRING;
      }
    }
    // future is null or done...
    return DeviceState.READY;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( Event aEvent )
  {
    String topic = aEvent.getTopic();
    if ( TOPIC_TASK_EXECUTION_STARTED.equals( topic ) )
    {
      String deviceName = ( String )aEvent.getProperty( TES_TASK_NAME );

      fireAcquisitionStartedEvent( deviceName );

      this.logService.log( LogService.LOG_INFO, "Device " + deviceName + " started acquisition..." );
    }
    else if ( TOPIC_TASK_EXECUTION_FINISHED.equals( topic ) )
    {
      String deviceName = ( String )aEvent.getProperty( TEF_TASK_NAME );
      AcquisitionData result = ( AcquisitionData )aEvent.getProperty( TEF_RESULT );
      Long executionTime = ( Long )aEvent.getProperty( TEF_EXECUTION_TIME );
      Exception exception = ( Exception )aEvent.getProperty( TEF_EXCEPTION );

      if ( exception != null )
      {
        // Failure...
        this.logService.log( LogService.LOG_WARNING, "Acquisition failed for device " + deviceName, exception );
      }
      else
      {
        this.logService.log( LogService.LOG_INFO, "Acquisition successful for device " + deviceName );
      }

      fireAcquisitionCompleteEvent( deviceName, executionTime, result, exception );

      this.acquisitionFutureTask = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAcquiring( String aDeviceName )
  {
    return ( this.acquisitionFutureTask != null ) && !this.acquisitionFutureTask.isCancelled()
        && !this.acquisitionFutureTask.isDone();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDeviceSetup( String aDeviceName )
  {
    return this.deviceConfigs.get( aDeviceName ) != null;
  }

  /**
   * Removes a given device from this service.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to remove, cannot be <code>null</code>.
   */
  public void removeDevice( Device aDevice )
  {
    this.devices.remove( aDevice.getName(), aDevice );
  }

  /**
   * Closes/shuts down this data acquisition service.
   * <p>
   * Called by the dependency manager.
   * </p>
   */
  public void stop()
  {
    if ( ( this.acquisitionFutureTask != null ) && !this.acquisitionFutureTask.isDone() )
    {
      this.acquisitionFutureTask.cancel( true /* mayInterruptIfRunning */);
      this.acquisitionFutureTask = null;
    }
  }

  /**
   * @param result
   */
  private void fireAcquisitionCompleteEvent( String aDeviceName, Long aExecutionTime, AcquisitionData aResult,
      Exception aException )
  {
    Map<Object, Object> props = new Properties();
    props.put( TAC_DEVICE_NAME, aDeviceName );
    props.put( TAC_EXECUTION_TIME, aExecutionTime );
    if ( aResult != null )
    {
      props.put( TAC_DATA, aResult );
    }
    if ( aException != null )
    {
      props.put( TAC_EXCEPTION, aException );
    }

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_FINISHED, props ) );
  }

  /**
   * @param aPercentage
   */
  private void fireAcquisitionInProgressEvent( String aDeviceName, int aPercentage )
  {
    Map<Object, Object> props = new Properties();
    props.put( TAP_DEVICE_NAME, aDeviceName );
    props.put( TAP_PROGRESS, aPercentage );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_PROGRESS, props ) );
  }

  /**
   * @param status
   */
  private void fireAcquisitionStartedEvent( String aDeviceName )
  {
    Map<Object, Object> props = new Properties();
    props.put( TAS_DEVICE_NAME, aDeviceName );

    this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_STARTED, props ) );
  }
}
