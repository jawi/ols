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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.io;


import java.util.*;

import nl.lxtreme.ols.io.serial.*;
import nl.lxtreme.ols.io.socket.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.io.*;
import org.osgi.service.log.*;


/**
 * Provides a bundle activator, that registers the ConnectorService
 * implementation as service.
 */
public class Activator extends DependencyActivatorBase
{
  // VARIABLES

  private ConnectorServiceImpl connectorService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    if ( this.connectorService != null )
    {
      this.connectorService.shutdown();
      this.connectorService = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( final BundleContext aContext, final DependencyManager aManager ) throws Exception
  {
    Hashtable<String, Object> dictionary = new Hashtable<String, Object>();

    // Connector service...
    this.connectorService = new ConnectorServiceImpl( aContext );

    aManager.add( createComponent() //
        .setInterface( ConnectorService.class.getName(), null ) //
        .setImplementation( this.connectorService ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );

    // Serial/comm connection factory...
    dictionary.put( ConnectionFactory.IO_SCHEME, new String[] { CommConnectionFactory.SCHEME } );

    aManager.add( createComponent() //
        .setInterface( ConnectionFactory.class.getName(), dictionary ) //
        .setImplementation( new CommConnectionFactory() ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );

    // Network connection factory...
    dictionary.put( ConnectionFactory.IO_SCHEME, new String[] { SocketConnectionFactory.SCHEME } );

    aManager.add( createComponent() //
        .setInterface( ConnectionFactory.class.getName(), dictionary ) //
        .setImplementation( new SocketConnectionFactory() ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );
  }
}
