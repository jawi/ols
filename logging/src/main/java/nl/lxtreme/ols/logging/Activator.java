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
package nl.lxtreme.ols.logging;


import java.util.logging.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;
import org.osgi.util.tracker.*;


/**
 * 
 */
public class Activator implements BundleActivator, ServiceTrackerCustomizer
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( Activator.class.getName() );

  // VARIABLES

  private BundleContext context;
  private JdkLogForwarder jdkLogForwarder;
  private ServiceTracker logReaderTracker;

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTrackerCustomizer#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aServiceRef )
  {
    LogReaderService reader = null;
    if ( aServiceRef != null )
    {
      reader = ( LogReaderService )this.context.getService( aServiceRef );
      if ( LOG.isLoggable( Level.INFO ) )
      {
        LOG.info( "Installing OSGi log reader..." );
      }
      reader.addLogListener( new SimpleLogReader() );
    }
    return reader;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTrackerCustomizer#modifiedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void modifiedService( final ServiceReference aServiceRef, final Object aService )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.osgi.util.tracker.ServiceTrackerCustomizer#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aServiceRef, final Object aService )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  public void start( final BundleContext aContext ) throws Exception
  {
    this.context = aContext;
    this.logReaderTracker = new ServiceTracker( aContext, LogReaderService.class.getName(), this );
    this.logReaderTracker.open();

    // The JDK Log Forwarder takes a default log handler as an argument which
    // will be used in the case the OSGi Log Service was not available.
    // The handle argument is optional.
    final ConsoleHandler defaultHandler = new ConsoleHandler();

    this.jdkLogForwarder = new JdkLogForwarder( aContext, defaultHandler, "nl.lxtreme.ols", "org.sump.device" );
    this.jdkLogForwarder.start();
  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  public void stop( final BundleContext aContext ) throws Exception
  {
    this.logReaderTracker.close();
    this.jdkLogForwarder.stop();
    this.context = null;
  }
}
