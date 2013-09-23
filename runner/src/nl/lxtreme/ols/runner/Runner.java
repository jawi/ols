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


import java.io.*;
import java.util.*;

import org.apache.felix.framework.*;
import org.apache.felix.framework.util.*;
import org.osgi.framework.*;


/**
 * Provides a main entry point for starting the OLS client from the command
 * line.
 */
public final class Runner
{
  // VARIABLES

  private Felix framework;
  private Logger logger;

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
    final String pluginDir = getPluginDir();
    final String binaryDir = getBinaryDir();
    final String bundleCacheDir = getBundleCacheDir();

    final Runner runner = new Runner();
    runner.run( pluginDir, binaryDir, bundleCacheDir );
    runner.waitForStop();
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

  // METHODS

  /**
   * Determines the bundle cache directory.
   * 
   * @return the fully qualified path to the directory with 'bundle caches',
   *         never <code>null</code>.
   * @throws IOException
   *           in case an I/O problem occurred during determining the cache
   *           path.
   */
  private static String getBundleCacheDir() throws IOException
  {
    final File pluginDir = new File( getPluginDir() );
    return new File( pluginDir.getParentFile(), "felix-cache" ).getCanonicalPath();
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

    String pluginProperty = System.getProperty( "nl.lxtreme.ols.bundle.dir", "./plugins" );
    pluginDir = new File( pluginProperty );
    if ( pluginDir.exists() && pluginDir.isDirectory() )
    {
      return pluginDir.getCanonicalPath();
    }
    else
    {
      pluginDir = new File( pluginDir, "plugins" );
      if ( pluginDir.exists() && pluginDir.isDirectory() )
      {
        return pluginDir.getCanonicalPath();
      }
    }

    throw new RuntimeException( "Failed to find plugins folder! Is '-Dnl.lxtreme.ols.bundle.dir' specified?" );
  }

  /**
   * @param pluginDir
   * @param binaryDir
   * @param bundleCacheDir
   * @throws Exception
   */
  public void run( final String pluginDir, final String binaryDir, final String bundleCacheDir ) throws Exception
  {
    try
    {
      this.framework = new Felix( createConfig( bundleCacheDir ) );
      this.framework.init();
      this.framework.start();

      // Issue #36: log something about where we're trying to read/store stuff,
      // makes offline debugging a bit easier...
      logger.log( Logger.LOG_INFO, "Framework started..." );
      logger.log( Logger.LOG_INFO, "  plugin dir: " + pluginDir );
      logger.log( Logger.LOG_INFO, "  binary dir: " + binaryDir );
      logger.log( Logger.LOG_INFO, "  cache dir : " + bundleCacheDir );

      BundleContext context = this.framework.getBundleContext();

      Map<String, Bundle> installed = getInstalledBundles( context );

      List<String> plugins = getPlugins( pluginDir );
      for ( String plugin : plugins )
      {
        if ( !installed.containsKey( plugin ) )
        {
          logger.log( Logger.LOG_DEBUG, "Installing plugin: '" + plugin + "'..." );

          Bundle bundle = installBundle( context, plugin );
          if ( bundle != null )
          {
            installed.put( plugin, bundle );
          }
        }
      }

      // Remove all installed plugins that are no longer available as plugin...
      List<String> removed = new ArrayList<String>( installed.keySet() );
      removed.remove( Constants.SYSTEM_BUNDLE_LOCATION );
      removed.removeAll( plugins );
      for ( String plugin : removed )
      {
        Bundle bundle = installed.remove( plugin );
        uninstallBundle( bundle );
      }

      // Start all remaining bundles...
      for ( Bundle bundle : installed.values() )
      {
        startBundle( bundle );
      }

      logger.log( Logger.LOG_INFO, "Bootstrap complete..." );
    }
    catch ( Exception exception )
    {
      logger.log( Logger.LOG_ERROR, "Failed to start OSGi framework! Possible reason: " + exception.getMessage(),
          exception );

      throw exception;
    }
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

  private Map<String, Object> createConfig( final String bundleCacheDir )
  {
    final HostActivator hostActivator = new HostActivator();
    final String logLevel = isDebugMode() ? "4" : "2";

    this.logger = new Logger();

    final Map<String, Object> config = new HashMap<String, Object>();

    config.put( FelixConstants.SYSTEMBUNDLE_ACTIVATORS_PROP, Arrays.asList( hostActivator ) );
    config.put( Constants.FRAMEWORK_SYSTEMPACKAGES_EXTRA, "com.apple.mrj,com.apple.eawt,javax.swing,javax.media.jai" );
    // Issue #36: explicitly set the location to the bundle cache directory,
    // otherwise it is created /relatively/ to the current working directory,
    // which is problematic when you start the client with a relative path...
    config.put( Constants.FRAMEWORK_STORAGE, bundleCacheDir );
    config.put( FelixConstants.LOG_LOGGER_PROP, this.logger );
    config.put( FelixConstants.LOG_LEVEL_PROP, logLevel );

    return config;
  }

  private Map<String, Bundle> getInstalledBundles( final BundleContext context )
  {
    Map<String, Bundle> installed = new HashMap<String, Bundle>();
    for ( Bundle bundle : context.getBundles() )
    {
      installed.put( bundle.getLocation(), bundle );
    }
    return installed;
  }

  private List<String> getPlugins( final String pluginDir )
  {
    final List<String> plugins = new ArrayList<String>();
    new File( pluginDir ).listFiles( new FileFilter()
    {
      @Override
      public boolean accept( File aFile )
      {
        if ( aFile.getName().endsWith( ".jar" ) )
        {
          plugins.add( aFile.toURI().toString() );
        }
        return false;
      }
    } );
    return plugins;
  }

  private Bundle installBundle( BundleContext context, String plugin )
  {
    try
    {
      return context.installBundle( plugin );
    }
    catch ( BundleException exception )
    {
      this.logger.log( Logger.LOG_WARNING, "Failed to install bundle: " + plugin + "...", exception );
      return null;
    }
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
    return Boolean.getBoolean( "nl.lxtreme.ols.client.debug" );
  }

  private void startBundle( Bundle bundle )
  {
    if ( ( bundle.getState() & Bundle.ACTIVE ) == 0 )
    {
      logger.log( Logger.LOG_DEBUG, "Starting bundle: " + bundle.getSymbolicName() + "..." );

      try
      {
        bundle.start();
      }
      catch ( BundleException exception )
      {
        this.logger.log( Logger.LOG_WARNING, "Failed to start bundle: " + bundle.getSymbolicName() + "...", exception );
      }
    }
  }

  private void uninstallBundle( Bundle bundle )
  {
    logger.log( Logger.LOG_DEBUG, "Removing stale plugin: " + bundle.getSymbolicName() + "..." );

    try
    {
      bundle.stop();
    }
    catch ( BundleException exception )
    {
      this.logger.log( Logger.LOG_WARNING, "Failed to stop bundle: " + bundle.getSymbolicName() + "...", exception );
    }
    try
    {
      bundle.uninstall();
    }
    catch ( BundleException exception )
    {
      this.logger
          .log( Logger.LOG_WARNING, "Failed to uninstall bundle: " + bundle.getSymbolicName() + "...", exception );
    }
  }
}

/* EOF */
