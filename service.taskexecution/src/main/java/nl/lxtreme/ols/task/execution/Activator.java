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


import java.util.*;

import nl.lxtreme.ols.api.task.*;

import org.osgi.framework.*;


/**
 * Provides a bundle activator for the tool execution service.
 */
public class Activator implements BundleActivator
{
  // VARIABLES

  private TaskStatusListenerHelper taskStatusListenerHelper;

  private BackgroundTaskExecutionService toolExecutionService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.taskStatusListenerHelper = new TaskStatusListenerHelper( aContext );
    this.taskStatusListenerHelper.open( true /* trackAllServices */);

    this.toolExecutionService = new BackgroundTaskExecutionService( this.taskStatusListenerHelper );

    final Dictionary<String, Object> props = new Hashtable<String, Object>();
    props.put( "invocation", "asynchonous" );

    aContext.registerService( TaskExecutionService.class.getName(), this.toolExecutionService, props );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    this.taskStatusListenerHelper.close();

    this.toolExecutionService.close();
  }

}
