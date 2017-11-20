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


import static org.apache.felix.resolver.Logger.*;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;

import org.apache.felix.framework.*;
import org.apache.felix.framework.util.*;
import org.osgi.framework.*;
import org.osgi.service.log.*;
import org.osgi.service.packageadmin.*;


/**
 * Provides a main entry point for starting the OLS client from the command
 * line.
 */
public final class Runner
{
  // INNER CLASSES

  static class CmdLineOptions
  {
    // VARIABLES

    final File pluginDir;
    final File cacheDir;
    final boolean cleanCache;
    final int logLevel;

    // CONSTRUCTORS

    public CmdLineOptions( final String... aCmdLineArgs ) throws IOException
    {
      String _pluginDir = getPluginDir();
      String _cacheDir = null;
      boolean _cleanCache = false;
      int _logLevel = 2;

      for ( String cmdLineArg : aCmdLineArgs )
      {
        if ( "-clean".equals( cmdLineArg ) )
        {
          _cleanCache = true;
        }
        else if ( cmdLineArg.startsWith( "-logLevel=" ) )
        {
          String arg = cmdLineArg.substring( 10 );
          _logLevel = Integer.parseInt( arg );
        }
        else if ( cmdLineArg.startsWith( "-pluginDir=" ) )
        {
          _pluginDir = cmdLineArg.substring( 11 );
        }
        else if ( cmdLineArg.startsWith( "-cacheDir=" ) )
        {
          _cacheDir = cmdLineArg.substring( 10 );
        }
      }

      if ( ( _logLevel < 0 ) || ( _logLevel > 6 ) )
      {
        throw new IllegalArgumentException( "Invalid log level, should be between 0 and 6!" );
      }

      this.pluginDir = new File( _pluginDir ).getCanonicalFile();
      if ( "".equals( _pluginDir ) || !this.pluginDir.exists() )
      {
        throw new IllegalArgumentException( String.format( "Invalid plugin directory (%s)!", this.pluginDir ) );
      }

      if ( _cacheDir == null )
      {
        _cacheDir = new File( _pluginDir, "/../.fwcache" ).getAbsolutePath();
      }

      this.cacheDir = new File( _cacheDir ).getCanonicalFile();
      if ( !this.cacheDir.exists() && !this.cacheDir.mkdirs() )
      {
        throw new IllegalArgumentException(
            String.format( "Invalid cache directory (%s): cannot create directory!", this.cacheDir ) );
      }

      this.cleanCache = _cleanCache;
      this.logLevel = _logLevel;
    }

    // METHODS

    /**
     * Searches for the plugins directory.
     * <p>
     * This method will take the system property
     * <tt>nl.lxtreme.ols.bundle.dir</tt> into consideration.
     * </p>
     *
     * @return the fully qualified path to the directory with plugins, can be
     *         <code>null</code> in case it could not be determined.
     */
    private static String getPluginDir()
    {
      String defaultDir = "./plugins";
      File pluginDir;

      pluginDir = new File( System.getProperty( "nl.lxtreme.ols.bundle.dir", defaultDir ) );
      if ( pluginDir.exists() && pluginDir.isDirectory() )
      {
        return pluginDir.getAbsolutePath();
      }

      pluginDir = new File( defaultDir );
      if ( pluginDir.exists() && pluginDir.isDirectory() )
      {
        return pluginDir.getAbsolutePath();
      }

      return null;
    }
  }

  // VARIABLES

  private final CmdLineOptions options;

  private Felix framework;
  private Logger fwLogger;

  // CONSTRUCTORS

  /**
   * Creates a new {@link Runner} instance.
   *
   * @throws IOException
   */
  public Runner( final CmdLineOptions aOptions ) throws IOException
  {
    this.options = aOptions;
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
    Runner runner = new Runner( new CmdLineOptions( aArgs ) );
    runner.run();
    runner.waitForStop();
  }

  /**
   * Bootstraps the OSGi framework and bootstraps the application.
   */
  public void run() throws Exception
  {
    try
    {
      this.framework = new Felix( createConfig() );
      this.framework.init();
      this.framework.start();

      // Issue #36: log something about where we're trying to read/store stuff,
      // makes offline debugging a bit easier...
      this.fwLogger.log( LogService.LOG_INFO, "Framework started..." );
      this.fwLogger.log( LogService.LOG_INFO, "  plugin dir: " + this.options.pluginDir );
      this.fwLogger.log( LogService.LOG_INFO, "  cache dir : " + this.options.cacheDir );

      bootstrap( this.framework.getBundleContext() );

      this.fwLogger.log( LogService.LOG_INFO, "Bootstrap complete..." );
    }
    catch ( Exception exception )
    {
      this.fwLogger.log( LogService.LOG_ERROR,
          "Failed to start OSGi framework! Possible reason: " + exception.getMessage(), exception );

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
    FrameworkEvent event = this.framework.waitForStop( 0 );
    switch ( event.getType() )
    {
      case FrameworkEvent.STOPPED:
        System.exit( 0 );
      case FrameworkEvent.ERROR:
      case FrameworkEvent.WARNING:
        System.exit( 1 );
      default:
        System.exit( -1 );
    }
  }

  /**
   * Bootstraps the application by installing the new/updated bundles and
   * removing all stale bundles.
   *
   * @param aContext
   *          the OSGi bundle context to use, cannot be <code>null</code>.
   */
  private void bootstrap( final BundleContext aContext ) throws InterruptedException
  {
    Map<String, Bundle> installed = getInstalledBundles( aContext );
    List<Bundle> toBeStarted = new ArrayList<Bundle>();

    List<String> available = getBundles( this.options.pluginDir );
    for ( String bundleLocation : available )
    {
      Bundle bundle = installed.get( bundleLocation );
      if ( bundle == null )
      {
        // New plugin...
        bundle = installBundle( aContext, bundleLocation );
        if ( bundle != null )
        {
          installed.put( bundleLocation, bundle );
          toBeStarted.add( bundle );
        }
      }
      else
      {
        // Plugin exists...
        File file = new File( URI.create( bundleLocation ) );
        if ( file.lastModified() >= bundle.getLastModified() )
        {
          bundle = updateBundle( bundle );
          if ( bundle != null )
          {
            toBeStarted.add( bundle );
          }
        }
      }
    }

    // Remove all installed plugins that are no longer available as plugin...
    List<String> removed = new ArrayList<String>( installed.keySet() );
    removed.remove( Constants.SYSTEM_BUNDLE_LOCATION );
    removed.removeAll( available );
    for ( String plugin : removed )
    {
      Bundle bundle = installed.remove( plugin );
      uninstallBundle( bundle );
    }

    refreshAll( aContext );

    // Start all remaining bundles...
    for ( Bundle bundle : toBeStarted )
    {
      startBundle( bundle );
    }
  }

  private Map<String, Object> createConfig()
  {
    this.fwLogger = new Logger();
    this.fwLogger.setLogger( this );

    final String logLevel = "" + Math.min( 4, this.options.logLevel );
    final Map<String, Object> config = new HashMap<String, Object>();

    config.put( Constants.FRAMEWORK_BOOTDELEGATION, "com.yourkit.*,com.sun.*,sun.*,apple.*,com.apple.*" );
    config.put( Constants.FRAMEWORK_SYSTEMPACKAGES_EXTRA,
        "com.apple.mrj,com.apple.eawt,javax.swing,javax.media.jai,org.osgi.service.cm" );
    // Issue #36: explicitly set the location to the bundle cache directory,
    // otherwise it is created /relatively/ to the current working directory,
    // which is problematic when you start the client with a relative path...
    config.put( Constants.FRAMEWORK_STORAGE, this.options.cacheDir.getPath() );
    if ( this.options.cleanCache )
    {
      config.put( Constants.FRAMEWORK_STORAGE_CLEAN, Constants.FRAMEWORK_STORAGE_CLEAN_ONFIRSTINIT );
    }
    // Felix specific configuration options...
    config.put( FelixConstants.SYSTEMBUNDLE_ACTIVATORS_PROP, Arrays.asList( new HostActivator() ) );
    config.put( FelixConstants.LOG_LEVEL_PROP, logLevel );
    config.put( FelixConstants.LOG_LOGGER_PROP, this.fwLogger );

    config.put( "nl.lxtreme.ols.config.dir", this.options.pluginDir.getAbsolutePath() );

    return config;
  }

  private List<String> getBundles( final File pluginDir )
  {
    final List<String> plugins = new ArrayList<String>();
    pluginDir.listFiles( new FilenameFilter()
    {
      @Override
      public boolean accept( final File aDir, final String aName )
      {
        if ( aName.endsWith( ".jar" ) )
        {
          plugins.add( new File( aDir, aName ).toURI().toString() );
        }
        return false;
      }
    } );
    return plugins;
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

  private Bundle installBundle( final BundleContext context, final String plugin )
  {
    this.fwLogger.log( LogService.LOG_DEBUG, "Installing plugin: '" + plugin + "'..." );

    try
    {
      return context.installBundle( plugin );
    }
    catch ( BundleException exception )
    {
      this.fwLogger.log( LogService.LOG_WARNING, "Failed to install bundle: " + plugin + "...", exception );
      return null;
    }
  }

  /**
   * @param aBundle
   *          the bundle to test, can be <code>null</code>.
   * @return <code>true</code> if the given bundle is a fragment bundle,
   *         <code>false</code> otherwise.
   */
  private boolean isFragment( final Bundle aBundle )
  {
    if ( aBundle == null )
    {
      return false;
    }
    return aBundle.getHeaders().get( Constants.FRAGMENT_HOST ) != null;
  }

  public void log( final int aLevel, final String aMessage, Throwable aException )
  {
    StringBuilder sb = new StringBuilder();
    switch ( aLevel )
    {
      case LOG_DEBUG:
        sb.append( "DEBUG" );
        break;
      case LOG_ERROR:
        sb.append( "ERROR" );
        break;
      case LOG_INFO:
        sb.append( "INFO" );
        break;
      case LOG_WARNING:
        sb.append( "WARNING" );
        break;
      default:
        sb.append( "UNKNOWN[" + aLevel + "]" );
        break;
    }
    sb.append( ": " ).append( aMessage );
    if ( aException != null )
    {
      if ( ( aException instanceof BundleException )
          && ( ( ( BundleException )aException ).getNestedException() != null ) )
      {
        aException = ( ( BundleException )aException ).getNestedException();
      }
      sb.append( " " ).append( aException.getMessage() );
    }

    System.err.println( sb.toString() );

    if ( aException != null )
    {
      aException.printStackTrace();
    }
  }

  @SuppressWarnings( "deprecation" )
  private void refreshAll( final BundleContext aContext ) throws InterruptedException
  {
    ServiceReference<?> ref = aContext.getServiceReference( PackageAdmin.class.getName() );
    if ( ref != null )
    {
      PackageAdmin packageAdm = ( PackageAdmin )aContext.getService( ref );

      final CountDownLatch packagesRefreshed = new CountDownLatch( 1 );
      FrameworkListener fwListener = new FrameworkListener()
      {
        @Override
        public void frameworkEvent( final FrameworkEvent aEvent )
        {
          switch ( aEvent.getType() )
          {
            case FrameworkEvent.ERROR:
            case FrameworkEvent.WAIT_TIMEDOUT:
            case FrameworkEvent.PACKAGES_REFRESHED:
              packagesRefreshed.countDown();
              break;
          }
        }
      };

      try
      {
        aContext.addFrameworkListener( fwListener );

        packageAdm.refreshPackages( null );

        if ( packagesRefreshed.await( 15, TimeUnit.SECONDS ) )
        {
          if ( !packageAdm.resolveBundles( null ) )
          {
            this.fwLogger.log( LogService.LOG_WARNING, "Not all bundles resolve correctly!" );
          }
        }
        else
        {
          throw new RuntimeException( "Refresh packages took longer than expected!" );
        }
      }
      finally
      {
        aContext.removeFrameworkListener( fwListener );
        aContext.ungetService( ref );
      }
    }
  }

  private void startBundle( final Bundle aBundle )
  {
    if ( !isFragment( aBundle ) )
    {
      this.fwLogger.log( LogService.LOG_DEBUG, "Starting bundle: " + aBundle.getSymbolicName() + "..." );

      try
      {
        aBundle.start( Bundle.START_ACTIVATION_POLICY );
      }
      catch ( BundleException exception )
      {
        this.fwLogger.log( LogService.LOG_WARNING, "Failed to start bundle: " + aBundle.getSymbolicName() + "...",
            exception );
      }
    }
  }

  private void uninstallBundle( final Bundle aBundle )
  {
    this.fwLogger.log( LogService.LOG_DEBUG, "Removing stale plugin: " + aBundle.getSymbolicName() + "..." );

    try
    {
      aBundle.stop();
    }
    catch ( BundleException exception )
    {
      this.fwLogger.log( LogService.LOG_WARNING, "Failed to stop bundle: " + aBundle.getSymbolicName() + "...",
          exception );
    }
    try
    {
      aBundle.uninstall();
    }
    catch ( BundleException exception )
    {
      this.fwLogger.log( LogService.LOG_WARNING, "Failed to uninstall bundle: " + aBundle.getSymbolicName() + "...",
          exception );
    }
  }

  private Bundle updateBundle( final Bundle aBundle )
  {
    this.fwLogger.log( LogService.LOG_DEBUG, "Updating plugin: " + aBundle.getSymbolicName() + "..." );

    Bundle result = null;
    if ( !isFragment( aBundle ) && ( ( aBundle.getState() & Bundle.ACTIVE ) != 0 ) )
    {
      this.fwLogger.log( LogService.LOG_DEBUG, "Stopping plugin: " + aBundle.getSymbolicName() + " for update..." );

      try
      {
        aBundle.stop();
      }
      catch ( BundleException exception )
      {
        this.fwLogger.log( LogService.LOG_WARNING, "Failed to stop bundle: " + aBundle.getSymbolicName() + "...",
            exception );
      }
      result = aBundle;
    }

    try
    {
      aBundle.update();
    }
    catch ( BundleException exception )
    {
      this.fwLogger.log( LogService.LOG_WARNING, "Failed to update bundle: " + aBundle.getSymbolicName() + "...",
          exception );
    }
    return result;
  }
}

/* EOF */
