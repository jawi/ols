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
package nl.lxtreme.ols.util.internal;


import java.util.logging.Logger;

import nl.lxtreme.ols.util.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;


/**
 * Bundle activator.
 */
public class Activator extends DependencyActivatorBase
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    // No-op
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    final HostProperties hostProperties = new HostResourceProperties( aContext );

    logEnvironment( hostProperties );

    aManager.add( createComponent() //
        .setInterface( HostProperties.class.getName(), null ) //
        .setImplementation( hostProperties ) //
    );

    aManager.add( createComponent() //
        .setImplementation( PlatformCallbackHelper.class ) //
        .add( createServiceDependency().setService( PlatformCallback.class )
            .setCallbacks( "setPlatformCallback", "removePlatformCallback" ).setRequired( false ) ) );
  }

  /**
   * @param aContext
   */
  private void logEnvironment( final HostProperties aProperties )
  {
    final String name = aProperties.getShortName();
    final String osName = aProperties.getOSName();
    final String osVersion = aProperties.getOSVersion();
    final String processor = aProperties.getProcessor();
    final String javaVersion = aProperties.getExecutionEnvironment();

    StringBuilder sb = new StringBuilder();
    sb.append( name ).append( " running on " ).append( osName ).append( ", " ).append( osVersion ).append( " (" )
        .append( processor ).append( "); " ).append( javaVersion ).append( "." );

    Logger.getLogger( getClass().getName() ).info( sb.toString() );
  }

}
