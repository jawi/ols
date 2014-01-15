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


import static nl.lxtreme.ols.common.OlsConstants.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.AcquisitionResultStatus.ResultStatus;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.event.*;


/**
 * Provides a {@link DataAcquisitionService} that performs the acquisition in
 * the background.
 */
public class BackgroundDataAcquisitionService implements DataAcquisitionService, EventHandler
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( BackgroundDataAcquisitionService.class.getName() );

  // VARIABLES

  private final List<AcquisitionProgressListener> acquisitionProgressListeners;
  private final List<AcquisitionStatusListener> acquisitionStatusListeners;
  private final ConcurrentMap<String, Device> devices;
  private final Map<String, Map<String, ? extends Serializable>> deviceConfigs;

  private volatile Future<?> acquisitionFutureTask;
  private volatile EventAdmin eventAdmin;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BackgroundDataAcquisitionService} instance.
   */
  public BackgroundDataAcquisitionService()
  {
    this.acquisitionProgressListeners = new CopyOnWriteArrayList<AcquisitionProgressListener>();
    this.acquisitionStatusListeners = new CopyOnWriteArrayList<AcquisitionStatusListener>();

    this.deviceConfigs = new HashMap<String, Map<String, ? extends Serializable>>();
    this.devices = new ConcurrentHashMap<String, Device>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( Map<String, ? extends Serializable> aConfig, String aDeviceName ) throws IOException
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
      @Override
      public void acquisitionInProgress( final int aPercentage )
      {
        fireAcquisitionInProgressEvent( aPercentage );
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

    Map<String, ? extends Serializable> config = this.deviceConfigs.get( aDeviceName );
    if ( config == null )
    {
      throw new IllegalStateException( "No configuration present for " + aDeviceName );
    }

    acquireData( config, aDeviceName );
  }

  /**
   * Adds a new {@link AcquisitionProgressListener} to the list of listeners.
   * <p>
   * Called by the dependency manager.
   * </p>
   * 
   * @param aListener
   *          the listener to add.
   */
  public void addAcquisitionProgressListener( final AcquisitionProgressListener aListener )
  {
    this.acquisitionProgressListeners.add( aListener );
  }

  /**
   * Adds a new {@link AcquisitionStatusListener} to the list of listeners.
   * <p>
   * Called by the dependency manager.
   * </p>
   * 
   * @param aListener
   *          the listener to add.
   */
  public void addAcquisitionStatusListener( final AcquisitionStatusListener aListener )
  {
    this.acquisitionStatusListeners.add( aListener );
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

    try
    {
      this.acquisitionFutureTask.cancel( true /* mayInterruptIfRunning */);
    }
    finally
    {
      this.acquisitionFutureTask = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, ? extends Serializable> configureDevice( java.awt.Window aParent, String aDeviceName )
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
  public boolean isAcquiring( String aDeviceName )
  {
    return ( this.acquisitionFutureTask != null ) && !this.acquisitionFutureTask.isDone();
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
   * Removes a given {@link AcquisitionProgressListener} from the list of
   * listeners.
   * <p>
   * Called by the dependency manager.
   * </p>
   * 
   * @param aListener
   *          the listener to remove.
   */
  public void removeAcquisitionProgressListener( final AcquisitionProgressListener aListener )
  {
    this.acquisitionProgressListeners.remove( aListener );
  }

  /**
   * Removes a given {@link AcquisitionStatusListener} from the list of
   * listeners.
   * <p>
   * Called by the dependency manager.
   * </p>
   * 
   * @param aListener
   *          the listener to remove.
   */
  public void removeAcquisitionStatusListener( final AcquisitionStatusListener aListener )
  {
    this.acquisitionStatusListeners.remove( aListener );
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
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( Event aEvent )
  {
    String status = ( String )aEvent.getProperty( "status" );
    if ( "success".equals( status ) )
    {
      AcquisitionData result = ( AcquisitionData )aEvent.getProperty( "result" );
      Long timeTaken = ( Long )aEvent.getProperty( "time" );

      fireAcquisitionCompleteEvent( result );

      fireAcquisitionEndedEvent( new AcquisitionResultStatus( ResultStatus.NORMAL, timeTaken ) );

      LOG.log( Level.INFO, "Acquisition successful!" );
    }
    else if ( "failure".equals( status ) )
    {
      Exception exception = ( Exception )aEvent.getProperty( "exception" );
      Long timeTaken = ( Long )aEvent.getProperty( "time" );

      fireAcquisitionEndedEvent( AcquisitionResultStatus.create( exception, timeTaken ) );

      LOG.log( Level.WARNING, "Acquisition failed!", exception );

    }
    else if ( "started".equals( status ) )
    {
      fireAcquisitionStartedEvent();
    }
  }

  /**
   * @param result
   */
  void fireAcquisitionCompleteEvent( AcquisitionData result )
  {
    if ( this.eventAdmin != null )
    {
      Map<Object, Object> props = new Properties();
      props.put( TAC_DATA, result );

      this.eventAdmin.postEvent( new Event( TOPIC_ACQUISITION_COMPLETE, props ) );
    }
  }

  /**
   * @param status
   */
  void fireAcquisitionEndedEvent( final AcquisitionResultStatus status )
  {
    final Iterator<AcquisitionStatusListener> statusListenerIter = this.acquisitionStatusListeners.iterator();
    while ( statusListenerIter.hasNext() )
    {
      statusListenerIter.next().acquisitionEnded( status );
    }
  }

  /**
   * @param aPercentage
   */
  void fireAcquisitionInProgressEvent( final int aPercentage )
  {
    Iterator<AcquisitionProgressListener> iter = this.acquisitionProgressListeners.iterator();
    while ( iter.hasNext() )
    {
      iter.next().acquisitionInProgress( aPercentage );
    }
  }

  /**
   * @param status
   */
  void fireAcquisitionStartedEvent()
  {
    final Iterator<AcquisitionStatusListener> statusListenerIter = this.acquisitionStatusListeners.iterator();
    while ( statusListenerIter.hasNext() )
    {
      statusListenerIter.next().acquisitionStarted();
    }
  }
}
