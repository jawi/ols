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
package nl.lxtreme.ols.tool.base;


import java.util.concurrent.*;

import nl.lxtreme.ols.api.task.*;
import nl.lxtreme.ols.util.osgi.*;

import org.osgi.framework.*;


/**
 * Provides a service tracker for {@link TaskExecutionService}s.
 */
public class TaskExecutionServiceTracker implements TaskExecutionService
{
  // VARIABLES

  private final WhiteboardHelper<TaskExecutionService> taskExecutionServiceHelper;

  // CONSTRUCTORS

  /**
   * Creates a new TaskExecutionServiceTracker instance.
   * 
   * @param aContext
   *          the bundle context to use, cannot be <code>null</code>.
   */
  public TaskExecutionServiceTracker( final BundleContext aContext )
  {
    this.taskExecutionServiceHelper = new WhiteboardHelper<TaskExecutionService>( aContext, TaskExecutionService.class );
  }

  // METHODS

  /**
   * Closes this task execution service.
   */
  public void close()
  {
    try
    {
      this.taskExecutionServiceHelper.close();
    }
    catch ( IllegalStateException exception )
    {
      // Ignore; bundle context probably is incorrect...
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RESULT_TYPE> Future<RESULT_TYPE> execute( final Task<RESULT_TYPE> aTask )
  {
    final TaskExecutionService service = this.taskExecutionServiceHelper.getService();
    if ( service != null )
    {
      return service.execute( aTask );
    }

    return null;
  }

  /**
   * Opens this task execution service for business.
   */
  public void open()
  {
    this.taskExecutionServiceHelper.open();
  }
}
