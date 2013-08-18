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
package nl.lxtreme.ols.api.task;


/**
 * Callback interface to listen for state changes of tasks.
 */
public interface TaskStatusListener
{
  // METHODS

  /**
   * Called when a task is ended normally.
   * 
   * @param aTask
   *          the task that is ended, never <code>null</code>;
   * @param aResult
   *          the result of the task, can be <code>null</code>.
   */
  <RT> void taskEnded( Task<RT> aTask, RT aResult );

  /**
   * Called when a task is started.
   * 
   * @param aTask
   *          the task that is started, never <code>null</code>;
   * @param aException
   *          the exception that was thrown when the task failed, never
   *          <code>null</code>.
   */
  <RT> void taskFailed( Task<RT> aTask, Exception aException );

  /**
   * Called when a task is started.
   * 
   * @param aTask
   *          the task that is started, never <code>null</code>.
   */
  <RT> void taskStarted( Task<RT> aTask );

}
