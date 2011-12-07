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
package nl.lxtreme.ols.acquisition;


import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.acquisition.AcquisitionResultStatus.ResultStatus;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.task.*;


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

  private volatile TaskExecutionService taskExecutionService;
  private volatile Future<?> acquisitionFutureTask;
  private volatile Task<AcquisitionResult> acquisitionTask;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BackgroundDataAcquisitionService} instance.
   */
  public BackgroundDataAcquisitionService()
  {
    this.acquisitionProgressListeners = new CopyOnWriteArrayList<AcquisitionProgressListener>();
    this.acquisitionStatusListeners = new CopyOnWriteArrayList<AcquisitionStatusListener>();
    this.acquisitionDataListeners = new CopyOnWriteArrayList<AcquisitionDataListener>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( final Device aDevice ) throws IOException
  {
    final AcquisitionTask innerTask = aDevice.createAcquisitionTask( new AcquisitionProgressListener()
    {
      @Override
      public void acquisitionInProgress( final int aPercentage )
      {
        fireAcquisitionInProgressEvent( aPercentage );
      }
    } );

    // Wrap the actual acquisition task in order to get a kind of "auto"
    // closable behavior...
    this.acquisitionTask = new Task<AcquisitionResult>()
    {
      @Override
      public AcquisitionResult call() throws Exception
      {
        try
        {
          return innerTask.call();
        }
        finally
        {
          aDevice.close();
        }
      }
    };

    this.acquisitionFutureTask = this.taskExecutionService.execute( this.acquisitionTask );
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

    final CancelTask cancelTask = aDevice.createCancelTask();
    if ( cancelTask != null )
    {
      this.taskExecutionService.execute( cancelTask );
    }
    else
    {
      this.acquisitionFutureTask.cancel( true /* mayInterruptIfRunning */);
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
    if ( aTask == this.acquisitionTask )
    {
      this.acquisitionTask = null;
      this.acquisitionFutureTask = null;

      final AcquisitionResult result = ( AcquisitionResult )aResult;
      fireAcquisitionCompleteEvent( result );

      final AcquisitionResultStatus status = new AcquisitionResultStatus( ResultStatus.NORMAL );
      fireAcquisitionEndedEvent( status );

      LOG.log( Level.INFO, "Acquisition successful!" );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RT> void taskFailed( final Task<RT> aTask, final Exception aException )
  {
    if ( aTask == this.acquisitionTask )
    {
      this.acquisitionTask = null;
      this.acquisitionFutureTask = null;

      final AcquisitionResultStatus status = AcquisitionResultStatus.create( aException );
      fireAcquisitionEndedEvent( status );

      LOG.log( Level.WARNING, "Acquisition failed!", aException );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RT> void taskStarted( final Task<RT> aTask )
  {
    if ( this.acquisitionTask == aTask )
    {
      fireAcquisitionStartedEvent();
    }
  }

  /**
   * @param result
   */
  void fireAcquisitionCompleteEvent( final AcquisitionResult result )
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
