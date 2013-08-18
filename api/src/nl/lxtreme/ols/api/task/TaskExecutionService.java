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


import java.util.concurrent.*;


/**
 * Provides a service that can execute tasks.
 */
public interface TaskExecutionService
{
  // METHODS

  /**
   * Executes the given task.
   * 
   * @param aTask
   *          the task to execute, cannot be <code>null</code>.
   * @return a {@link Future} that can be used to track the result of the task,
   *         can not be <code>null</code>.
   */
  <RESULT_TYPE> Future<RESULT_TYPE> execute( Task<RESULT_TYPE> aTask );

}
