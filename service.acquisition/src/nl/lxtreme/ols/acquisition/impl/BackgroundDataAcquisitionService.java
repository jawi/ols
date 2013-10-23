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


import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.acquisition.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.AcquisitionResultStatus.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.task.execution.*;


/**
 * Provides a {@link DataAcquisitionService} that performs the acquisition in
 * the background.
 */
public class BackgroundDataAcquisitionService implements DataAcquisitionService, TaskStatusListener
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( BackgroundDataAcquisitionService.class.getName() );

  // VARIABLES

  private final List<AcquisitionProgressListener> acquisitionProgressListeners;
  private final List<AcquisitionStatusListener> acquisitionStatusListeners;
  private final List<AcquisitionDataListener> acquisitionDataListeners;
  private final Map<String, Map<String, ? extends Serializable>> deviceConfigs;

  private volatile Future<?> acquisitionFutureTask;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BackgroundDataAcquisitionService} instance.
   */
  public BackgroundDataAcquisitionService()
  {
    this.acquisitionProgressListeners = new CopyOnWriteArrayList<AcquisitionProgressListener>();
    this.acquisitionStatusListeners = new CopyOnWriteArrayList<AcquisitionStatusListener>();
    this.acquisitionDataListeners = new CopyOnWriteArrayList<AcquisitionDataListener>();

    this.deviceConfigs = new HashMap<String, Map<String, ? extends Serializable>>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( Device aDevice ) throws IOException
  {
    if ( aDevice == null )
    {
      throw new IllegalArgumentException( "Device cannot be null!" );
    }

    Map<String, ? extends Serializable> config = this.deviceConfigs.get( aDevice.getName() );
    if ( config == null )
    {
      throw new IllegalStateException( "No configuration present for " + aDevice.getName() );
    }

    acquireData( config, aDevice );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( Map<String, ? extends Serializable> aConfig, Device aDevice ) throws IOException
  {
    if ( aConfig == null )
    {
      throw new IllegalArgumentException( "Config cannot be null!" );
    }
    if ( aDevice == null )
    {
      throw new IllegalArgumentException( "Device cannot be null!" );
    }
    if ( isAcquiring() )
    {
      throw new IllegalStateException( "Acquisition still in progress!" );
    }

    // Keep this configuration for later use...
    this.deviceConfigs.put( aDevice.getName(), aConfig );

    this.acquisitionFutureTask = aDevice.acquireData( aConfig, new AcquisitionProgressListener()
    {
      @Override
      public void acquisitionInProgress( final int aPercentage )
      {
        fireAcquisitionInProgressEvent( aPercentage );
      }
    } );
  }

  /**
   * Adds a new {@link AcquisitionDataListener} to the list of listeners.
   * <p>
   * Called by the dependency manager.
   * </p>
   * 
   * @param aListener
   *          the listener to add.
   */
  public void addAcquisitionDataListener( final AcquisitionDataListener aListener )
  {
    this.acquisitionDataListeners.add( aListener );
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
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition( final Device aDevice ) throws IOException, IllegalStateException
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
  public boolean isAcquiring()
  {
    return ( this.acquisitionFutureTask != null ) && !this.acquisitionFutureTask.isDone();
  }

  /**
   * Removes a given {@link AcquisitionDataListener} from the list of listeners.
   * <p>
   * Called by the dependency manager.
   * </p>
   * 
   * @param aListener
   *          the listener to remove.
   */
  public void removeAcquisitionDataListener( final AcquisitionDataListener aListener )
  {
    this.acquisitionDataListeners.remove( aListener );
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
  public <RT> void taskEnded( final Task<RT> aTask, final RT aResult )
  {
    this.acquisitionFutureTask = null;

    final AcquisitionData result = ( AcquisitionData )aResult;
    fireAcquisitionCompleteEvent( result );

    final AcquisitionResultStatus status = new AcquisitionResultStatus( ResultStatus.NORMAL );
    fireAcquisitionEndedEvent( status );

    LOG.log( Level.INFO, "Acquisition successful!" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RT> void taskFailed( final Task<RT> aTask, final Exception aException )
  {
    this.acquisitionFutureTask = null;

    final AcquisitionResultStatus status = AcquisitionResultStatus.create( aException );
    fireAcquisitionEndedEvent( status );

    LOG.log( Level.WARNING, "Acquisition failed!", aException );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RT> void taskStarted( final Task<RT> aTask )
  {
    fireAcquisitionStartedEvent();
  }

  /**
   * @param result
   */
  void fireAcquisitionCompleteEvent( final AcquisitionData result )
  {
    final Iterator<AcquisitionDataListener> dataListenerIter = this.acquisitionDataListeners.iterator();
    while ( dataListenerIter.hasNext() )
    {
      dataListenerIter.next().acquisitionComplete( result );
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
