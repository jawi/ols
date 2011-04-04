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


import org.osgi.framework.*;
import org.osgi.service.io.ConnectorService;


/**
 * Provides a bundle activator, that registers the ConnectorService
 * implementation as service.
 */
public class Activator implements BundleActivator
{
  // VARIABLES

  private ConnectorServiceImpl connectorService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.connectorService = new ConnectorServiceImpl( aContext );

    aContext.registerService( ConnectorService.class.getName(), this.connectorService, null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    if ( this.connectorService != null )
    {
      this.connectorService.shutdown();
      this.connectorService = null;
    }
  }
}
