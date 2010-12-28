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
package nl.lxtreme.ols.runner;


import java.util.*;

import nl.lxtreme.ols.util.*;

import org.apache.felix.framework.*;
import org.apache.felix.framework.util.*;
import org.apache.felix.main.*;
import org.osgi.framework.*;


/**
 * Provides a main entry point for starting the OLS client from the command
 * line.
 */
public final class Runner
{
  // VARIABLES

  private final HostActivator hostActivator;
  private Felix framework;

  // CONSTRUCTORS

  /**
   * Creates a new Runner instance.
   */
  public Runner()
  {
    final Map<String, Object> config = new HashMap<String, Object>();

    final String pluginDir = System.getProperty( "nl.lxtreme.ols.bundle.dir", "./plugins" );

    this.hostActivator = new HostActivator();
    final List<BundleActivator> activators = new ArrayList<BundleActivator>();
    activators.add( this.hostActivator );

    config.put( FelixConstants.SYSTEMBUNDLE_ACTIVATORS_PROP, activators );
    config.put( Constants.FRAMEWORK_SYSTEMPACKAGES_EXTRA, "com.apple.eawt,javax.media.jai" );
    config.put( AutoProcessor.AUTO_DEPLOY_ACTION_PROPERY, "install,start" );
    config.put( AutoProcessor.AUTO_DEPLOY_DIR_PROPERY, pluginDir );
    if ( Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) ) )
    {
      config.put( Constants.FRAMEWORK_STORAGE_CLEAN, Constants.FRAMEWORK_STORAGE_CLEAN_ONFIRSTINIT );
    }
    config.put( Constants.FRAMEWORK_BEGINNING_STARTLEVEL, "4" );
    config.put( FelixConstants.BUNDLE_STARTLEVEL_PROP, "1" );
    config.put( FelixConstants.LOG_LEVEL_PROP, "1" );

    try
    {
      this.framework = new Felix( config );
      this.framework.init();

      AutoProcessor.process( config, this.framework.getBundleContext() );

      this.framework.start();
    }
    catch ( Exception exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        System.err.println( "Failed to start OSGi framework! Possible reason: " + exception.getMessage() );
        exception.printStackTrace();
      }

      System.exit( -1 );
    }
  }

  // METHODS

  /**
   * MAIN ENTRY POINT
   * 
   * @param aArgs
   *          the (optional) command line arguments, can be empty but never
   *          <code>null</code>.
   */
  public static void main( final String[] aArgs ) throws Exception
  {
    final Runner runner = new Runner();
    runner.waitForStop();
  }

  /**
   * Waits until the OSGi framework is shut down.
   * 
   * @throws InterruptedException
   */
  public void waitForStop() throws InterruptedException
  {
    this.framework.waitForStop( 0 );
  }
}

/* EOF */
