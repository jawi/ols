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


import java.awt.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.data.project.*;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.HostUtils.ApplicationCallback;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.framework.*;


/**
 * Denotes the client host.
 */
public final class Host implements ApplicationCallback
{
  // CONSTANT

  private static final Logger LOG = Logger.getLogger( Host.class.getName() );

  public static final String SHORT_NAME = "LogicSniffer";
  public static final String FULL_NAME = SHORT_NAME.concat( " - Logic Analyzer Client" );

  /** The name of the implicit user settings properties file name. */
  private static final String IMPLICIT_USER_SETTING_NAME = "nl.lxtreme.ols.client";

  // VARIABLES

  private final BundleContext context;
  private final Properties clientProperties;

  private DeviceControllerTracker deviceControllerTracker;
  private MenuTracker menuTracker;
  private PreferenceServiceTracker preferencesServiceTracker;
  private ExporterTracker exporterTracker;
  private ToolTracker toolTracker;
  private ClientController controller;
  private ProjectManager projectManager;

  // CONSTRUCTORS

  /**
   * Creates a new Host instance.
   * 
   * @param aBundleContext
   *          the bundle context to use.
   */
  public Host( final BundleContext aBundleContext ) throws IOException
  {
    if ( aBundleContext == null )
    {
      throw new IllegalArgumentException( "Bundle context cannot be null!" );
    }
    this.context = aBundleContext;

    this.clientProperties = new Properties();

    // Try to load the embedded properties...
    URL resource = aBundleContext.getBundle().getResource( "/client.properties" );
    if ( resource != null )
    {
      InputStream is = null;

      try
      {
        is = resource.openStream();

        this.clientProperties.load( is );
      }
      finally
      {
        if ( is != null )
        {
          is.close();
        }
        resource = null;
      }
    }
  }

  // METHODS

  /**
   * Returns a symbolic name for this host.
   * 
   * @return a symbolic name, never <code>null</code>.
   */
  public static final String getShortName()
  {
    return SHORT_NAME;
  }

  /**
   * Exits this host by stopping the framework bundle.
   */
  public void exit()
  {
    try
    {
      // Store the implicit user settings...
      saveImplicitUserSettings( this.projectManager );

      // Stop the framework bundle; which should stop all other bundles as
      // well...
      this.context.getBundle( 0 ).stop();
    }
    catch ( IllegalStateException ex )
    {
      // The bundle context is no longer valid; we're going to exit anyway, so
      // lets ignore this exception for now...
      System.exit( -1 );
    }
    catch ( BundleException be )
    {
      System.exit( -1 );
    }
  }

  /**
   * Returns the email address to use for reporting incidents.
   * 
   * @return a report incident email address, can be <code>null</code>.
   */
  public final String getReportIncidentAddress()
  {
    return ( String )this.clientProperties.get( "client.incidentAddress" );
  }

  /**
   * Returns this client's version.
   * 
   * @return a version String, never <code>null</code>.
   */
  public final String getVersion()
  {
    return String.valueOf( this.clientProperties.get( "client.version" ) );
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleAbout()
   */
  @Override
  public boolean handleAbout()
  {
    this.controller.showAboutBox();
    return true;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handlePreferences()
   */
  @Override
  public boolean handlePreferences()
  {
    this.controller.showPreferencesDialog( this.controller.getMainFrame() );
    return true;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleQuit()
   */
  @Override
  public boolean handleQuit()
  {
    exit();
    // On Mac OS, it appears that if we acknowledge this event, the system
    // shuts down our application for us, thereby not calling our stop/shutdown
    // hooks... By returning false, we're not acknowledging the quit action to
    // the system, but instead do it all on our own...
    return false;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#hasPreferences()
   */
  @Override
  public boolean hasPreferences()
  {
    return true;
  }

  /**
   * Initializes this host application. May <em>not</em> be called from outside
   * the EDT.
   */
  public void initialize()
  {
    if ( isDebugMode() )
    {
      // Install a custom repaint manager that detects whether Swing components
      // are created outside the EDT; if so, it will yield a stack trace to the
      // offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
    }

    // Use the defined email address...
    System.setProperty( JErrorDialog.PROPERTY_REPORT_INCIDENT_EMAIL_ADDRESS, getReportIncidentAddress() );

    // Cause exceptions to be shown in a more user-friendly way...
    JErrorDialog.installSwingExceptionHandler();

    this.projectManager = new SimpleProjectManager( this );
    // Restore the implicit user settings...
    loadImplicitUserSettings( this.projectManager );

    this.controller = new ClientController( this.context, this, this.projectManager );

    final MainFrame mainFrame = new MainFrame( this.controller );
    this.controller.setMainFrame( mainFrame );

    this.preferencesServiceTracker = new PreferenceServiceTracker( this.context, this.projectManager );
    this.deviceControllerTracker = new DeviceControllerTracker( this.context, this.controller );
    this.exporterTracker = new ExporterTracker( this.context, this.controller );
    this.menuTracker = new MenuTracker( this.context, mainFrame.getJMenuBar() );
    this.toolTracker = new ToolTracker( this.context, this.controller );

    LOG.log( Level.FINE, "{0} initialized ...", SHORT_NAME );
  }

  /**
   * Shutdown hook, called after {@link #stop()} is called and can be used to
   * write down preferences and such. This method may <em>not</em> be called
   * from outside the EDT.
   */
  public void shutdown()
  {
    JErrorDialog.uninstallSwingExceptionHandler();

    LOG.log( Level.FINE, "{0} shutting down ...", SHORT_NAME );
  }

  /**
   * Starts this host by making the main frame visible, may <em>not</em> be
   * called from outside the EDT.
   */
  public void start()
  {
    this.preferencesServiceTracker.open();
    this.deviceControllerTracker.open();
    this.exporterTracker.open();
    this.toolTracker.open();
    this.menuTracker.open();

    final MainFrame mainFrame = this.controller.getMainFrame();
    if ( mainFrame != null )
    {
      mainFrame.setVisible( true );

      this.controller.setStatus( "{0} v{1} ready ...", SHORT_NAME, getVersion() );
    }

    LOG.log( Level.INFO, "{0} v{1} started ...", new String[] { SHORT_NAME, getVersion() } );

    final String osName = this.context.getProperty( Constants.FRAMEWORK_OS_NAME );
    final String osVersion = this.context.getProperty( Constants.FRAMEWORK_OS_VERSION );
    final String processor = this.context.getProperty( Constants.FRAMEWORK_PROCESSOR );

    LOG.log( Level.INFO, "  running on {0}, {1} ({2}).", new String[] { osName, osVersion, processor } );
  }

  /**
   * Stops this host by making the main frame invisible, may <em>not</em> be
   * called from outside the EDT.
   */
  public void stop()
  {
    // First stop all OSGi related services; then close down all Swing windows.
    // This way, we ensure that the cleanup occurs in an orderly fashion and
    // does not cause "weird" exceptions upon shutdown...
    this.preferencesServiceTracker.close();
    this.deviceControllerTracker.close();
    this.toolTracker.close();
    this.menuTracker.close();

    MainFrame mainFrame = this.controller.getMainFrame();
    if ( mainFrame != null )
    {
      // Safety guard: also loop through all unclosed frames and close them as
      // well...
      final Window[] openWindows = Window.getWindows();
      for ( Window window : openWindows )
      {
        LOG.log( Level.FINE, "(Forced) closing window {0} ...", window );

        window.setVisible( false );
        window.dispose();
      }

      this.controller.setMainFrame( mainFrame = null );
    }

    LOG.log( Level.INFO, "{0} stopped ...", SHORT_NAME );
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

  /**
   * @param aProjectManager
   */
  private void loadImplicitUserSettings( final ProjectManager aProjectManager )
  {
    final File userSettingsFile = HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME, "settings" );
    if ( userSettingsFile.exists() )
    {
      InputStream is = null;

      try
      {
        is = new FileInputStream( userSettingsFile );

        aProjectManager.loadProject( is );
      }
      catch ( IOException exception )
      {
        LOG.log( Level.WARNING, "Failed to load implicit user settings...", exception );
      }
      finally
      {
        HostUtils.closeResource( is );
      }
    }
  }

  /**
   * @param aProjectManager
   */
  private void saveImplicitUserSettings( final ProjectManager aProjectManager )
  {
    final File userSettingsFile = HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME, "settings" );
    OutputStream is = null;

    try
    {
      is = new FileOutputStream( userSettingsFile );

      aProjectManager.saveProject( is );
    }
    catch ( IOException exception )
    {
      LOG.log( Level.WARNING, "Failed to save implicit user settings...", exception );
    }
    finally
    {
      HostUtils.closeResource( is );
    }
  }
}

/* EOF */
