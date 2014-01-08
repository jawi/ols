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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.annotations;


import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;


/**
 * Provides a timer that can be reset.
 * <p>
 * Taken and adapted from Apache Felix UserAdmin File-based store
 * implementation.
 * </p>
 */
final class ResettableTimer
{
  private final ScheduledExecutorService m_executor;
  private final Runnable m_task;
  private final long m_timeout;
  private final TimeUnit m_timeUnit;
  private final AtomicReference<ScheduledFuture<?>> m_futureRef;

  /**
   * Creates a new {@link ResettableTimer} calling a given task when a given
   * timeout exceeds.
   * 
   * @param executor
   *          the executor to use to execute the task, cannot be
   *          <code>null</code>;
   * @param task
   *          the task to execute upon timeout, cannot be <code>null</code>;
   * @param timeout
   *          the timeout value, > 0;
   * @param unit
   *          the time unit of the timeout value, cannot be <code>null</code>.
   */
  public ResettableTimer( Runnable task, long timeout, TimeUnit unit )
  {
    m_executor = Executors.newScheduledThreadPool( 1 );
    m_task = task;
    m_timeout = timeout;
    m_timeUnit = unit;

    m_futureRef = new AtomicReference<ScheduledFuture<?>>();
  }

  /**
   * Schedules the task for execution with the contained timeout. If a task is
   * already pending or running, it will be cancelled (not interrupted). The new
   * task will be scheduled to run in now + timeout.
   * 
   * @return <code>true</code> if the schedule was successful,
   *         <code>false</code> otherwise.
   */
  public boolean schedule()
  {
    ScheduledFuture<?> currentTask = cancelCurrentTask();
    if ( m_executor.isShutdown() )
    {
      // We cannot submit any new tasks...
      return false;
    }
    ScheduledFuture<?> newTask = m_executor.schedule( m_task, m_timeout, m_timeUnit );
    m_futureRef.compareAndSet( currentTask, newTask );
    return true;
  }

  /**
   * @return the current task, or <code>null</code> if no task is available.
   */
  private ScheduledFuture<?> cancelCurrentTask()
  {
    ScheduledFuture<?> currentTask = m_futureRef.get();
    if ( currentTask != null )
    {
      // Doesn't matter for completed tasks...
      currentTask.cancel( false /* mayInterruptIfRunning */);
    }
    return currentTask;
  }
}
