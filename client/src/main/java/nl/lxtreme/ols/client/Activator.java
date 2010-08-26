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
package nl.lxtreme.ols.client;


import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;


/**
 * 
 */
public class Activator implements BundleActivator
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( Activator.class.getName() );

  // VARIABLES

  private Host host = null;

  // METHODS

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.host = new Host( aContext );

    // This has to be done *before* any other Swing related code is executed
    // so this also means the #invokeLater call done below...
    HostUtils.initOSSpecifics( Host.getShortName(), this.host );

    final Runnable initTask = new Runnable()
    {
      @Override
      public void run()
      {
        LOG.fine( "Initializing client ..." );
        Activator.this.host.initialize();
      }
    };
    SwingUtilities.invokeAndWait( initTask );

    final Runnable startTask = new Runnable()
    {
      @Override
      public void run()
      {
        LOG.info( "Starting client ..." );
        Activator.this.host.start();
      }
    };
    SwingUtilities.invokeLater( startTask );
  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    final Runnable stopTask = new Runnable()
    {
      @Override
      public void run()
      {
        LOG.info( "Stopping client ..." );
        Activator.this.host.stop();
      }
    };
    SwingUtilities.invokeAndWait( stopTask );

    final Runnable shutdownTask = new Runnable()
    {
      @Override
      public void run()
      {
        LOG.fine( "Shutting down client ..." );
        Activator.this.host.shutdown();
      }
    };
    SwingUtilities.invokeAndWait( shutdownTask );
  }

}

/* EOF */
