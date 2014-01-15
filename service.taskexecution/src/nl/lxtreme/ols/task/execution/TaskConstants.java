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
package nl.lxtreme.ols.task.execution;


/**
 * Provides constants used in {@link TaskExecutionService}.
 */
public interface TaskConstants
{
  // METHODS

  /** Base topic for all task-related events. */
  String TOPIC_TASK_EXECUTION_BASE = "nl/lxtreme/ols/task/execution";

  /** Event topic for reporting the start of tasks. */
  String TOPIC_TASK_EXECUTION_STARTED = TOPIC_TASK_EXECUTION_BASE.concat( "/STARTED" );

  /** Event topic for reporting the end of tasks (successful or failure). */
  String TOPIC_TASK_EXECUTION_FINISHED = TOPIC_TASK_EXECUTION_BASE.concat( "/FINISHED" );
  
  /** String property denoting the name of a task used in {@link #TOPIC_TASK_EXECUTION_STARTED}. */
  String TES_TASK_NAME = "taskName";
  /** Starting (Epoch) timestamp of a task used in {@link #TOPIC_TASK_EXECUTION_STARTED}. */
  String TES_START_TIME = "startTime";

  /** String property denoting the name of a task used in {@link #TOPIC_TASK_EXECUTION_FINISHED}. */
  String TEF_TASK_NAME = TES_TASK_NAME;
  /** Denotes the execution time (in ms) of a task used in {@link #TOPIC_TASK_EXECUTION_FINISHED}. */
  String TEF_EXECUTION_TIME = "executionTime";
  /** Denotes the (optional) result of a task as used in {@link #TOPIC_TASK_EXECUTION_FINISHED}. */
  String TEF_RESULT = "result";
  /** Denotes the (optional) failure reason as used in {@link #TOPIC_TASK_EXECUTION_FINISHED}. */
  String TEF_EXCEPTION = "exception";

}
