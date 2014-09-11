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
package nl.lxtreme.ols.runner;


import nl.lxtreme.ols.runner.MiniLogger.LogTarget;

import org.osgi.framework.*;
import org.osgi.service.log.*;
import org.slf4j.impl.*;


/**
 * Provide a "stub" bundle activator for the runner.
 */
public class HostActivator implements BundleActivator
{
  // VARIABLES

  private final int logLevel;
  private final MiniLogger logger;

  // CONSTRUCTORS

  /**
   * Creates a new HostActivator instance.
   * 
   * @param aLogTarget
   * @param aLogLevel
   */
  public HostActivator( LogTarget aLogTarget, int aLogLevel )
  {
    this.logger = new MiniLogger( aLogTarget, aLogLevel );
    this.logLevel = aLogLevel;
  }

  // METHODS

  /**
   * Returns the current value of bridge.
   * 
   * @return the bridge
   */
  public LogService getLogger()
  {
    return this.logger;
  }

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    StaticLoggerBinder.getSingleton().bridgeTo( this.logger, this.logLevel );

    this.logger.start( aContext );
  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    this.logger.stop( aContext );

    StaticLoggerBinder.getSingleton().bridgeTo( null, -1 );
  }
}

/* EOF */
