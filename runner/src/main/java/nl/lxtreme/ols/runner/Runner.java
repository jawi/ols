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


import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.logging.Logger;

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
  // CONSTANTS

  private static final String[] AUTO_START_BUNDLES = { "org.apache.felix.configadmin", "org.apache.felix.fileinstall" };

  private static final Logger LOG = Logger.getLogger( Runner.class.getName() );

  // VARIABLES

  private final HostActivator hostActivator;
  private Felix framework;

  // CONSTRUCTORS

  /**
   * Creates a new Runner instance.
   */
  public Runner() throws Exception
  {
    final Map<String, Object> config = new HashMap<String, Object>();

    final String pluginDir = getPluginDir();
    final String binaryDir = getBinaryDir();

    // We only start a single bundle: the file install bundle; this bundle will
    // be responsible for starting all other bundles...
    final String autoStartBundles = getAutoInstallBundles( binaryDir );

    this.hostActivator = new HostActivator();
    final List<BundleActivator> activators = new ArrayList<BundleActivator>();
    activators.add( this.hostActivator );

    config.put( FelixConstants.SYSTEMBUNDLE_ACTIVATORS_PROP, activators );
    config.put( Constants.FRAMEWORK_SYSTEMPACKAGES_EXTRA, "com.apple.eawt,javax.swing,javax.media.jai" );
    config.put( AutoProcessor.AUTO_DEPLOY_ACTION_PROPERY, "install,start" );
    config.put( AutoProcessor.AUTO_START_PROP, autoStartBundles );
    if ( isDebugMode() )
    {
      config.put( Constants.FRAMEWORK_STORAGE_CLEAN, Constants.FRAMEWORK_STORAGE_CLEAN_ONFIRSTINIT );
    }
    config.put( Constants.FRAMEWORK_BEGINNING_STARTLEVEL, "4" );
    config.put( FelixConstants.BUNDLE_STARTLEVEL_PROP, "1" );
    config.put( FelixConstants.LOG_LEVEL_PROP, "1" );

    System.setProperty( "felix.fileinstall.noInitialDelay", Boolean.toString( true ) );
    System.setProperty( "felix.fileinstall.dir", pluginDir );
    System.setProperty( "felix.fileinstall.start.level", "2" );
    if ( isDebugMode() )
    {
      System.setProperty( "felix.fileinstall.log.level", "4" );
    }
    else
    {
      System.setProperty( "felix.fileinstall.log.level", "1" );
    }

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
        LOG.log( Level.SEVERE, "Failed to start OSGi framework! Possible reason: " + exception.getMessage() );
        LOG.log( Level.FINE, "Details: ", exception );
      }

      throw exception;
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
   * Returns a space-separated list of bundles that should be started by the
   * framework. These bundles should be present in the given directory.
   * Currently, this is only one bundle, the Felix file-install bundle, which
   * takes care of starting all other bundles.
   * 
   * @param aBinDir
   *          the binary directory, cannot be <code>null</code>.
   * @return the list of bundles that should be automatically started by the
   *         framework, never <code>null</code>.
   */
  private static String getAutoInstallBundles( final String aBinDir )
  {
    final File binDir = new File( aBinDir );

    final String[] autoInstallBundleNames = binDir.list( new FilenameFilter()
    {
      @Override
      public boolean accept( final File aDir, final String aName )
      {
        for ( String autoStartedBundleName : AUTO_START_BUNDLES )
        {
          if ( aName.startsWith( autoStartedBundleName ) )
          {
            return true;
          }
        }
        return false;
      }
    } );

    final StringBuilder result = new StringBuilder();
    if ( ( autoInstallBundleNames != null ) && ( autoInstallBundleNames.length > 0 ) )
    {
      for ( String autoInstallBundleName : autoInstallBundleNames )
      {
        if ( result.length() > 0 )
        {
          result.append( ' ' );
        }

        final File file = new File( binDir, autoInstallBundleName );
        result.append( '"' ).append( file.toURI() ).append( '"' );
      }
    }

    return result.toString();
  }

  /**
   * Determines the binary directory.
   * 
   * @return the fully qualified path to the directory with 'binaries', never
   *         <code>null</code>.
   * @throws IOException
   *           in case an I/O problem occurred during determining the binary
   *           path.
   */
  private static String getBinaryDir() throws IOException
  {
    final File pluginDir = new File( getPluginDir() );
    return new File( pluginDir.getParentFile(), "bin" ).getCanonicalPath();
  }

  /**
   * Searches for the plugins directory.
   * <p>
   * This method will take the system property
   * <tt>nl.lxtreme.ols.bundle.dir</tt> into consideration.
   * </p>
   * 
   * @return the fully qualified path to the directory with plugins, never
   *         <code>null</code>.
   * @throws IOException
   *           in case an I/O problem occurred during determining the plugins
   *           path.
   */
  private static String getPluginDir() throws IOException
  {
    File pluginDir;

    pluginDir = new File( System.getProperty( "user.dir" ), "plugins" );
    if ( pluginDir.exists() && pluginDir.isDirectory() )
    {
      return pluginDir.getCanonicalPath();
    }

    pluginDir = new File( "./plugins" );
    if ( pluginDir.exists() && pluginDir.isDirectory() )
    {
      return pluginDir.getCanonicalPath();
    }

    String pluginProperty = System.getProperty( "nl.lxtreme.ols.bundle.dir", "./plugins" );
    pluginDir = new File( pluginProperty );
    return pluginDir.getCanonicalPath();
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

  /**
   * Returns whether or not debugging is enabled.
   * <p>
   * Useful for additional checks, logging and so on.
   * </p>
   * 
   * @return <code>true</code> if debug mode is enabled, <code>false</code>
   *         otherwise.
   */
  private boolean isDebugMode()
  {
    return Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) );
  }
}

/* EOF */
