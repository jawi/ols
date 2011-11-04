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
package nl.lxtreme.ols.client.osgi;


import java.io.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.acquisition.AcquisitionResultStatus.ResultStatus;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.api.task.*;
import nl.lxtreme.ols.util.osgi.*;

import org.osgi.framework.*;


/**
 * 
 */
public class SimpleDataAcquisitionService implements DataAcquisitionService, TaskStatusListener
{
  // INNER TYPES

  /**
   * 
   */
  static final class DataAcquisitionDataListener implements AcquisitionDataListener
  {
    // VARIABLES

    private final WhiteboardHelper<AcquisitionDataListener> acquisitionDataListenerHelper;

    // CONSTRUCTORS

    /**
     * Creates a new DataAcquisitionDataListener instance.
     */
    public DataAcquisitionDataListener( final BundleContext aContext )
    {
      this.acquisitionDataListenerHelper = new WhiteboardHelper<AcquisitionDataListener>( aContext,
          AcquisitionDataListener.class );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void acquisitionComplete( final AcquisitionResult aData )
    {
      this.acquisitionDataListenerHelper.accept( new WhiteboardHelper.Visitor<AcquisitionDataListener>()
      {
        /**
         * {@inheritDoc}
         */
        @Override
        public void visit( final AcquisitionDataListener aService )
        {
          aService.acquisitionComplete( aData );
        }
      } );
    }

    /**
     * 
     */
    public void close()
    {
      this.acquisitionDataListenerHelper.close();
    }

    /**
     * 
     */
    public void open()
    {
      this.acquisitionDataListenerHelper.open( true /* trackAllServices */);
    }
  }

  /**
   * Helper for propagating {@link AcquisitionProgressListener} events to their
   * implementors.
   */
  static final class DataAcquisitionProgressListener implements AcquisitionProgressListener
  {
    // VARIABLES

    private final WhiteboardHelper<AcquisitionProgressListener> acquisitionProgressListenerHelper;

    // CONSTRUCTORS

    /**
     * Creates a new DataAcquisitionProgressListener instance.
     */
    public DataAcquisitionProgressListener( final BundleContext aContext )
    {
      this.acquisitionProgressListenerHelper = new WhiteboardHelper<AcquisitionProgressListener>( aContext,
          AcquisitionProgressListener.class );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void acquisitionInProgress( final int aPercentage )
    {
      this.acquisitionProgressListenerHelper.accept( new WhiteboardHelper.Visitor<AcquisitionProgressListener>()
      {
        /**
         * {@inheritDoc}
         */
        @Override
        public void visit( final AcquisitionProgressListener aService )
        {
          aService.acquisitionInProgress( aPercentage );
        }
      } );
    }

    /**
     * 
     */
    public void close()
    {
      this.acquisitionProgressListenerHelper.close();
    }

    /**
     * 
     */
    public void open()
    {
      this.acquisitionProgressListenerHelper.open( true /* trackAllServices */);
    }
  }

  /**
   * 
   */
  static final class DataAcquisitionStatusListener implements AcquisitionStatusListener
  {
    // VARIABLES

    private final WhiteboardHelper<AcquisitionStatusListener> acquisitionStatusListenerHelper;

    // CONSTRUCTORS

    /**
     * Creates a new DataAcquisitionStatusListener instance.
     */
    public DataAcquisitionStatusListener( final BundleContext aContext )
    {
      this.acquisitionStatusListenerHelper = new WhiteboardHelper<AcquisitionStatusListener>( aContext,
          AcquisitionStatusListener.class );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void acquisitionEnded( final AcquisitionResultStatus aStatus )
    {
      this.acquisitionStatusListenerHelper.accept( new WhiteboardHelper.Visitor<AcquisitionStatusListener>()
      {
        /**
         * {@inheritDoc}
         */
        @Override
        public void visit( final AcquisitionStatusListener aService )
        {
          aService.acquisitionEnded( aStatus );
        }
      } );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void acquisitionStarted()
    {
      this.acquisitionStatusListenerHelper.accept( new WhiteboardHelper.Visitor<AcquisitionStatusListener>()
      {
        /**
         * {@inheritDoc}
         */
        @Override
        public void visit( final AcquisitionStatusListener aService )
        {
          aService.acquisitionStarted();
        }
      } );
    }

    /**
     * 
     */
    public void close()
    {
      this.acquisitionStatusListenerHelper.close();
    }

    /**
     * 
     */
    public void open()
    {
      this.acquisitionStatusListenerHelper.open( true /* trackAllServices */);
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SimpleDataAcquisitionService.class.getName() );

  // VARIABLES

  private final WhiteboardHelper<TaskExecutionService> taskExecutionServiceHelper;
  private final DataAcquisitionProgressListener acquisitionProgressListener;
  private final DataAcquisitionStatusListener acquisitionStatusListener;
  private final DataAcquisitionDataListener acquisitionDataListener;
  private final ServiceRegistration serviceReg;

  private volatile Future<?> acquisitionFutureTask;
  private volatile AcquisitionTask acquisitionTask;

  // CONSTRUCTORS

  /**
   * Creates a new DataAcquisitionServiceTracker instance.
   * 
   * @param aContext
   *          the bundle context to use.
   */
  public SimpleDataAcquisitionService( final BundleContext aContext )
  {
    this.taskExecutionServiceHelper = new WhiteboardHelper<TaskExecutionService>( aContext, TaskExecutionService.class );
    this.acquisitionProgressListener = new DataAcquisitionProgressListener( aContext );
    this.acquisitionStatusListener = new DataAcquisitionStatusListener( aContext );
    this.acquisitionDataListener = new DataAcquisitionDataListener( aContext );

    this.serviceReg = aContext.registerService( TaskStatusListener.class.getName(), this, null );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquireData( final Device aDevice ) throws IOException
  {
    this.acquisitionTask = aDevice.createAcquisitionTask( this.acquisitionProgressListener );

    this.taskExecutionServiceHelper.acceptFirst( new WhiteboardHelper.Visitor<TaskExecutionService>()
    {
      @Override
      public void visit( final TaskExecutionService aService )
      {
        SimpleDataAcquisitionService.this.acquisitionFutureTask = aService
            .execute( SimpleDataAcquisitionService.this.acquisitionTask );
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancelAcquisition() throws IllegalStateException
  {
    if ( this.acquisitionFutureTask == null )
    {
      throw new IllegalStateException( "No acquisition in progress!" );
    }

    this.acquisitionFutureTask.cancel( true /* mayInterruptIfRunning */);
    this.acquisitionFutureTask = null;
  }

  /**
   * Closes/shuts down this data acquisition service.
   */
  public void close()
  {
    this.serviceReg.unregister();

    this.acquisitionProgressListener.close();
    this.acquisitionStatusListener.close();
    this.acquisitionDataListener.close();
    this.taskExecutionServiceHelper.close();
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
   * Opens this data acquisition service for business.
   */
  public void open()
  {
    this.acquisitionProgressListener.open();
    this.acquisitionStatusListener.open();
    this.acquisitionDataListener.open();
    this.taskExecutionServiceHelper.open();
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
      this.acquisitionDataListener.acquisitionComplete( result );

      this.acquisitionStatusListener.acquisitionEnded( new AcquisitionResultStatus( ResultStatus.NORMAL ) );

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

      this.acquisitionStatusListener.acquisitionEnded( AcquisitionResultStatus.create( aException ) );

      LOG.log( Level.WARNING, "Acquisition failed!", aException );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RT> void taskStarted( final Task<RT> aTask )
  {
    this.acquisitionStatusListener.acquisitionStarted();
  }
}
