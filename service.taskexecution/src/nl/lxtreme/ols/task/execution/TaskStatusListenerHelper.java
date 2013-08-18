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


import org.osgi.framework.*;

import nl.lxtreme.ols.api.task.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.osgi.*;


/**
 * Whiteboard helper for all {@link ToolProgressListener}s.
 */
public class TaskStatusListenerHelper extends WhiteboardHelper<TaskStatusListener> implements TaskStatusListener
{
  // CONSTRUCTORS

  /**
   * Creates a new ToolProgressListenerHelper instance.
   * 
   * @param aContext
   *          the current bundle context to use.
   */
  public TaskStatusListenerHelper( final BundleContext aContext )
  {
    super( aContext, TaskStatusListener.class );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public <RESULT_TYPE> void taskEnded( final Task<RESULT_TYPE> aTask, final RESULT_TYPE aResult )
  {
    final Object[] services = getServices();
    if ( services != null )
    {
      for ( Object service : services )
      {
        ( ( TaskStatusListener )service ).taskEnded( aTask, aResult );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RESULT_TYPE> void taskFailed( final Task<RESULT_TYPE> aTask, final Exception aException )
  {
    final Object[] services = getServices();
    if ( services != null )
    {
      for ( Object service : services )
      {
        ( ( TaskStatusListener )service ).taskFailed( aTask, aException );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <RESULT_TYPE> void taskStarted( final Task<RESULT_TYPE> aTask )
  {
    final Object[] services = getServices();
    if ( services != null )
    {
      for ( Object service : services )
      {
        ( ( TaskStatusListener )service ).taskStarted( aTask );
      }
    }
  }
}
