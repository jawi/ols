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
package nl.lxtreme.ols.acquisition.service.impl;


import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.acquisition.service.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Provides a {@link DataAcquisitionService} that performs the acquisition in
 * the background.
 */
public class BackgroundDataAcquisitionService implements DataAcquisitionService, DeviceProgressListener
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

  private volatile LogService logService;
  private volatile EventAdmin eventAdmin;
  private volatile Session session;

  private final ExecutorService executorService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BackgroundDataAcquisitionService} instance.
   */
  public BackgroundDataAcquisitionService()
  {
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
  public Future<AcquisitionData> acquireData( final Device aDevice ) throws IOException
  {
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
          final AcquisitionData result = aDevice.acquireData( BackgroundDataAcquisitionService.this );

          fireAcquisitionEndedEvent( name, result, this.startTime );

          return result;
        }
        catch ( Exception exception )
        {
          fireAcquisitionFailedEvent( name, exception, this.startTime );

          throw exception;
        }
        finally
        {
          aDevice.close();
        }
      }

      @Override
      public void cancel()
      {
        fireAcquisitionCancelledEvent( aDevice.getName(), this.startTime );
      }
    };

    return this.executorService.submit( task );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionInProgress( final int aPercentage )
  {
    final Event event = new Event( DataAcquisitionService.TOPIC_ACQUISITION_PROGRESS, //
        Collections.singletonMap( DataAcquisitionService.KEY_PROGRESS, Integer.valueOf( aPercentage ) ) );

    this.eventAdmin.postEvent( event );
  }

  /**
   * Closes/shuts down this data acquisition service.
   * <p>
   * Called by the dependency manager.
   * </p>
   */
  public void stop() throws InterruptedException
  {
    this.executorService.shutdownNow();
    this.executorService.awaitTermination( 5, TimeUnit.SECONDS );
  }

  /**
   * 
   */
  private void fireAcquisitionCancelledEvent( final String aDeviceName, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( DataAcquisitionService.KEY_STATUS, DataAcquisitionService.STATUS_CANCELLED );
    props.put( DataAcquisitionService.KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( DataAcquisitionService.KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( DataAcquisitionService.TOPIC_ACQUISITION_STATUS, props ) );

    this.logService.log( LogService.LOG_WARNING, "Acquisition cancelled for " + aDeviceName );
  }

  /**
   * @param aData
   */
  private void fireAcquisitionEndedEvent( final String aDeviceName, final AcquisitionData aData, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( DataAcquisitionService.KEY_STATUS, DataAcquisitionService.STATUS_SUCCESS );
    props.put( DataAcquisitionService.KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( DataAcquisitionService.KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( DataAcquisitionService.TOPIC_ACQUISITION_STATUS, props ) );

    this.session.setAcquisitionData( aData );

    this.logService.log( LogService.LOG_INFO, "Acquisition successful for " + aDeviceName );
  }

  /**
   * @param aException
   */
  private void fireAcquisitionFailedEvent( final String aDeviceName, final Throwable aException, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( DataAcquisitionService.KEY_STATUS, DataAcquisitionService.STATUS_FAILED );
    props.put( DataAcquisitionService.KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( DataAcquisitionService.KEY_EXCEPTION, aException );
    props.put( DataAcquisitionService.KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( DataAcquisitionService.TOPIC_ACQUISITION_STATUS, props ) );

    this.logService.log( LogService.LOG_WARNING, "Acquisition failed for " + aDeviceName, aException );
  }

  /**
   * Fires an event to notify listers that an acquisition is started.
   */
  private void fireAcquisitionStartedEvent( final String aDeviceName, final long aStartTime )
  {
    final Map<String, Object> props = new HashMap<String, Object>();
    props.put( DataAcquisitionService.KEY_STATUS, DataAcquisitionService.STATUS_STARTED );
    props.put( DataAcquisitionService.KEY_START_TIME, Long.valueOf( aStartTime ) );
    props.put( DataAcquisitionService.KEY_DEVICE, aDeviceName );

    this.eventAdmin.postEvent( new Event( DataAcquisitionService.TOPIC_ACQUISITION_STATUS, props ) );

    this.logService.log( LogService.LOG_WARNING, "Acquisition started for " + aDeviceName );
  }
}
