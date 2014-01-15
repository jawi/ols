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
package nl.lxtreme.ols.task.execution.impl;


import static nl.lxtreme.ols.task.execution.TaskConstants.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.task.execution.*;

import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Provides a {@link TaskExecutionService} that invokes {@link Task}s in a
 * background thread.
 */
public class BackgroundTaskExecutionService implements TaskExecutionService
{
  // VARIABLES

  // Locally managed...
  private volatile ExecutorService executorService;
  // Injected by Felix DM...
  private volatile EventAdmin eventAdmin;
  private volatile LogService logService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public <RESULT_TYPE> Future<RESULT_TYPE> execute( final Task<RESULT_TYPE> aTask, final Map<String, ?> aProperties )
  {
    if ( aTask == null )
    {
      throw new IllegalArgumentException( "Parameter Task cannot be null!" );
    }

    final Callable<RESULT_TYPE> task = new Callable<RESULT_TYPE>()
    {
      @Override
      public RESULT_TYPE call() throws Exception
      {
        long startTime = System.currentTimeMillis();

        fireTaskStarted( aTask, aProperties, startTime );

        try
        {
          RESULT_TYPE result = aTask.call();

          fireTaskEnded( aTask, result, aProperties, System.currentTimeMillis() - startTime );

          return result;
        }
        catch ( Exception exception )
        {
          logService.log( LogService.LOG_INFO, "Task execution failed!", exception );

          fireTaskFailed( aTask, exception, aProperties, System.currentTimeMillis() - startTime );

          throw exception;
        }
      }
    };

    return this.executorService.submit( task );
  }

  /**
   * Fires an event that a task is successfully ended.
   * 
   * @param aTask
   * @param aResult
   */
  final void fireTaskEnded( Task<?> aTask, Object aResult, Map<String, ?> aProperties, long aTimeTaken )
  {
    Map<String, Object> props = new HashMap<String, Object>();
    if ( aProperties != null )
    {
      props.putAll( aProperties );
    }
    props.put( TEF_TASK_NAME, aTask.getName() );
    props.put( TEF_RESULT, aResult );
    props.put( TEF_EXECUTION_TIME, aTimeTaken );

    this.eventAdmin.postEvent( new Event( TOPIC_TASK_EXECUTION_FINISHED, props ) );
  }

  /**
   * Fires an event that a task has failed.
   * 
   * @param aTask
   * @param aException
   */
  final void fireTaskFailed( Task<?> aTask, Exception aException, Map<String, ?> aProperties, long aTimeTaken )
  {
    Map<String, Object> props = new HashMap<String, Object>();
    if ( aProperties != null )
    {
      props.putAll( aProperties );
    }
    props.put( TEF_TASK_NAME, aTask.getName() );
    props.put( TEF_EXCEPTION, aException );
    props.put( TEF_EXECUTION_TIME, aTimeTaken );

    this.eventAdmin.postEvent( new Event( TOPIC_TASK_EXECUTION_FINISHED, props ) );
  }

  /**
   * Fires an event that a task has started.
   * 
   * @param aTask
   */
  final void fireTaskStarted( Task<?> aTask, Map<String, ?> aProperties, long aStartTime )
  {
    Map<String, Object> props = new HashMap<String, Object>();
    if ( aProperties != null )
    {
      props.putAll( aProperties );
    }
    props.put( TES_TASK_NAME, aTask.getName() );
    props.put( TES_START_TIME, aStartTime );

    this.eventAdmin.postEvent( new Event( TOPIC_TASK_EXECUTION_STARTED, props ) );
  }

  /**
   * @param aEventAdmin
   *          the event admin, can be <code>null</code>.
   */
  protected void setEventAdmin( EventAdmin aEventAdmin )
  {
    this.eventAdmin = aEventAdmin;
  }

  /**
   * @param aLogService
   *          the log service, can be <code>null</code>.
   */
  protected void setLogService( LogService aLogService )
  {
    this.logService = aLogService;
  }

  /**
   * Starts this task execution service.
   * 
   * @param aComponent
   *          the component that is started.
   */
  protected void start()
  {
    this.executorService = Executors.newCachedThreadPool();
  }

  /**
   * Closes this task execution service, cancelling any ongoing executions if
   * needed.
   * 
   * @param aComponent
   *          the component that is started.
   */
  protected void stop()
  {
    if ( this.executorService == null || this.executorService.isShutdown() )
    {
      return;
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
          this.logService.log( LogService.LOG_DEBUG, "All running threads are terminated ..." );
        }
      }
      catch ( InterruptedException exception )
      {
        // Make sure our thread administration is correct...
        Thread.currentThread().interrupt();
      }
    }

    this.executorService = null;

    this.logService.log( LogService.LOG_INFO, "Background task execution service closed ..." );
  }
}
