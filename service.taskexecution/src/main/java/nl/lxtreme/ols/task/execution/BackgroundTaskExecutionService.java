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
package nl.lxtreme.ols.task.execution;


import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.task.*;


/**
 * Provides a {@link TaskExecutionService} that invokes {@link Task}s in a
 * background thread.
 */
public class BackgroundTaskExecutionService implements TaskExecutionService
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( BackgroundTaskExecutionService.class.getName() );

  private static final int POOL_SIZE = 5;

  // VARIABLES

  private final TaskStatusListener taskStatusListener;

  final ExecutorService executorService;

  // CONSTANTS

  /**
   * Creates a new {@link BackgroundTaskExecutionService} instance.
   */
  public BackgroundTaskExecutionService( final TaskStatusListener aTaskStatusListener )
  {
    this.taskStatusListener = aTaskStatusListener;

    this.executorService = Executors.newFixedThreadPool( POOL_SIZE );
  }

  // METHODS

  /**
   * Closes this tool execution service, cancelling any ongoing executions if
   * needed.
   * <p>
   * After calling this method, this instance should <em>no longer</em> be used.
   * </p>
   */
  public void close()
  {
    if ( this.executorService.isShutdown() )
    {
      throw new IllegalStateException( "Service already shut down!" );
    }

    // Force the running tasks to be cancelled immediately...
    this.executorService.shutdownNow();

    int tries = 3;
    while ( !this.executorService.isTerminated() && ( tries-- >= 0 ) )
    {
      try
      {
        if ( this.executorService.awaitTermination( 500L, TimeUnit.MILLISECONDS ) )
        {
          LOG.fine( "All running threads are terminated ..." );
        }
      }
      catch ( InterruptedException exception )
      {
        // Make sure our thread administration is correct...
        Thread.currentThread().interrupt();
      }
    }

    LOG.fine( "Background task execution service closed ..." );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RESULT_TYPE> Future<RESULT_TYPE> execute( final Task<RESULT_TYPE> aTask )
  {
    if ( aTask == null )
    {
      throw new IllegalArgumentException( "Parameter Task cannot be null!" );
    }

    final Callable<RESULT_TYPE> task = new Callable<RESULT_TYPE>()
    {
      private final TaskStatusListener tsl = BackgroundTaskExecutionService.this.taskStatusListener;

      /**
       * {@inheritDoc}
       */
      @Override
      public RESULT_TYPE call() throws Exception
      {
        this.tsl.taskStarted( aTask );

        try
        {
          RESULT_TYPE result = aTask.call();
          this.tsl.taskEnded( aTask, result );
          return result;
        }
        catch ( Exception exception )
        {
          LOG.log( Level.FINE, "Task execution failed!", exception );
          this.tsl.taskFailed( aTask, exception );
          throw exception;
        }
      }
    };

    return this.executorService.submit( task );
  }
}
