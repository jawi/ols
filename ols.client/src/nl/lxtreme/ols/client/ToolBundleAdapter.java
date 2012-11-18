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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client;


import java.util.*;

import nl.lxtreme.ols.client.tool.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.tool.api.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.log.*;
import org.osgi.service.metatype.*;


/**
 * Provides a bundle adapter for handling tools.
 */
public class ToolBundleAdapter
{
  // CONSTANTS

  public static final String OLS_TOOL_MAGIC_KEY = "OLS-Tool";
  public static final String OLS_TOOL_MAGIC_VALUE = "1.0";
  public static final String OLS_TOOL_CLASS_KEY = "OLS-ToolClass";
  public static final String TOOL_BUNDLE_FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_TOOL_MAGIC_KEY,
      OLS_TOOL_MAGIC_VALUE, OLS_TOOL_CLASS_KEY );

  // VARIABLES

  // Injected by Felix DM...
  private volatile Bundle bundle; // = adapted bundle
  private volatile DependencyManager manager; // injected
  private volatile Component serviceComponent; // = added service

  // CONSTRUCTORS

  /**
   * Creates a new {@link ToolBundleAdapter} instance.
   */
  public ToolBundleAdapter()
  {
    // Nop
  }

  // METHODS

  /**
   * Called by Dependency Manager upon destruction of this component.
   */
  public void destroy( final Component aComponent ) throws Exception
  {
    if ( this.serviceComponent != null )
    {
      this.manager.remove( this.serviceComponent );
    }
  }

  /**
   * Called by Dependency Manager upon initialization of this component.
   */
  @SuppressWarnings( "unchecked" )
  public void init( final Component aComponent ) throws Exception
  {
    // Make sure to register services on behalf of the *original* bundle!
    this.manager = new DependencyManager( this.bundle.getBundleContext() );

    Dictionary<?, ?> bundleProps = this.bundle.getHeaders();

    String entry = ( String )bundleProps.get( OLS_TOOL_CLASS_KEY );
    if ( ( entry == null ) || "".equals( entry ) )
    {
      throw new IllegalArgumentException( "No such header given: " + OLS_TOOL_CLASS_KEY );
    }

    Class<Tool> toolImpl = this.bundle.loadClass( entry );
    String[] interfaces = { ToolInvoker.class.getName(), ManagedService.class.getName() };

    // NOTE: the service.pid property is set in the ToolInvokerImpl#init()
    // method as it is based on the metatype PID...

    this.serviceComponent = this.manager.createComponent() //
        .setInterface( interfaces, new Properties() ) //
        .setImplementation( new ToolInvokerImpl( toolImpl.newInstance() ) ) //
        .add( this.manager.createServiceDependency() //
            .setService( Session.class ) //
            .setRequired( true ) ) //
        .add( this.manager.createServiceDependency() //
            .setService( EventAdmin.class ) //
            .setRequired( true ) ) //
        .add( this.manager.createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        .add( this.manager.createServiceDependency() //
            .setService( ConfigurationAdmin.class ) //
            .setRequired( true ) ) //
        .add( this.manager.createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) );

    this.manager.add( this.serviceComponent );
  }
}
