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
package nl.lxtreme.ols.client.osgi;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.service.log.*;
import org.osgi.service.prefs.*;


/**
 * Provides an OSGi service tracker for preference services.
 */
public class UserSessionManager
{
  // INNER TYPES

  /**
   * Defines a (global) AWT event listener for storing/retrieving the window
   * state.
   */
  static class WindowStateListener implements AWTEventListener
  {
    // CONSTANTS

    /**
     * Client property that should be present on the rootpane of a dialog or
     * frame to make it unmanaged.
     */
    public static final String PROPERTY_UNMANAGED = "unmanaged";

    // VARIABLES

    private volatile PreferencesService preferenceService;
    private volatile ProjectManager projectManager;
    private volatile LogService logger;

    private final ConcurrentMap<String, Boolean> prefsLoaded;
    private final String userName;

    // CONSTRUCTORS

    /**
     * Creates a new {@link WindowStateListener} instance.
     */
    public WindowStateListener()
    {
      this.userName = System.getProperty( "user.name", "default" );

      this.prefsLoaded = new ConcurrentHashMap<String, Boolean>();
    }

    // METHODS

    /**
     * @see java.awt.event.AWTEventListener#eventDispatched(java.awt.AWTEvent)
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      final ComponentEvent event = ( ComponentEvent )aEvent;
      final Window component = ( Window )event.getComponent();
      final String namespace = component.getClass().getName();
      final int id = aEvent.getID();

      if ( id == WindowEvent.WINDOW_OPENED )
      {
        // When we've already loaded the preferences once; don't do this
        // again...
        if ( arePreferencesLoaded( namespace ) )
        {
          return;
        }

        loadPreferences( component, namespace );
      }
      else if ( id == WindowEvent.WINDOW_CLOSED )
      {
        // When we've already written the preferences once; don't do this
        // again...
        if ( arePreferencesSaved( namespace ) )
        {
          return;
        }

        savePreferences( component, namespace );
      }
    }

    /**
     * Called by Felix DM when this component is started.
     */
    public void start()
    {
      // Install us as a global window state listener...
      Toolkit.getDefaultToolkit().addAWTEventListener( this, AWTEvent.WINDOW_EVENT_MASK );

      this.logger.log( LogService.LOG_DEBUG, "AWT Window state listener installed..." );
    }

    /**
     * Called by Felix DM when this component is stopped.
     */
    public void stop()
    {
      Toolkit.getDefaultToolkit().removeAWTEventListener( this );

      this.logger.log( LogService.LOG_DEBUG, "AWT Window state listener removed..." );
    }

    /**
     * Sets logger to the given value.
     * 
     * @param aLogger
     *          the logger to set.
     */
    final void setLogger( final LogService aLogger )
    {
      this.logger = aLogger;
    }

    /**
     * Sets preferenceService to the given value.
     * 
     * @param aPreferenceService
     *          the preferenceService to set.
     */
    final void setPreferenceService( final PreferencesService aPreferenceService )
    {
      this.preferenceService = aPreferenceService;
    }

    /**
     * Sets projectManager to the given value.
     * 
     * @param aProjectManager
     *          the projectManager to set.
     */
    final void setProjectManager( final ProjectManager aProjectManager )
    {
      this.projectManager = aProjectManager;
    }

    /**
     * Returns whether or not the preferences are loaded for the given
     * namespace.
     * 
     * @param aNamespace
     *          the namespace key to check.
     * @return <code>true</code> if the (Window) preferences of the given
     *         namespace are already loaded once, <code>false</code> otherwise.
     */
    private boolean arePreferencesLoaded( final String aNamespace )
    {
      Boolean result = this.prefsLoaded.get( aNamespace );
      return Boolean.TRUE.equals( result );
    }

    /**
     * Returns whether or not the preferences are saved for the given namespace.
     * 
     * @param aNamespace
     *          the namespace key to check.
     * @return <code>true</code> if the (Window) preferences of the given
     *         namespace are already saved once, <code>false</code> otherwise.
     */
    private boolean arePreferencesSaved( final String aNamespace )
    {
      Boolean result = this.prefsLoaded.get( aNamespace );
      return Boolean.FALSE.equals( result );
    }

    /**
     * Returns the preferences node for the given namespace.
     * 
     * @param aNamespace
     *          the node namespace to retrieve the preferences for, cannot be
     *          <code>null</code>.
     * @return the preferences node for the given namespace, never
     *         <code>null</code>.
     */
    private Preferences getUserPreferences( final String aNamespace )
    {
      final Preferences userPreferences = this.preferenceService.getUserPreferences( this.userName );
      return userPreferences.node( aNamespace );
    }

    /**
     * Returns the user settings of the current project for the given name
     * space.
     * 
     * @param aNamespace
     *          the node name space to retrieve the preferences for, cannot be
     *          <code>null</code>.
     * @return the preferences node for the given name space, never
     *         <code>null</code>.
     */
    private UserSettings getUserSettings( final String aNamespace )
    {
      return this.projectManager.getCurrentProject().getSettings( aNamespace );
    }

    /**
     * Returns whether or not the given component is a managed window.
     * 
     * @param aComponent
     *          the component to test, can be <code>null</code>.
     * @return <code>false</code> if the given component is not a managed window
     *         (or <code>null</code>), <code>true</code> if it is.
     */
    private boolean isManagedWindow( final Window aComponent )
    {
      if ( aComponent instanceof JFrame )
      {
        if ( !( ( JFrame )aComponent ).isUndecorated() )
        {
          return false;
        }

        Object prop = ( ( JFrame )aComponent ).getRootPane().getClientProperty( PROPERTY_UNMANAGED );
        if ( prop != null )
        {
          return false;
        }

        return true;
      }
      if ( aComponent instanceof JDialog )
      {
        if ( !( ( JDialog )aComponent ).isUndecorated() )
        {
          return false;
        }

        Object prop = ( ( JDialog )aComponent ).getRootPane().getClientProperty( PROPERTY_UNMANAGED );
        if ( prop != null )
        {
          return false;
        }

        return true;
      }

      return false;
    }

    /**
     * Loads the window preferences for the given window with the given
     * namespace.
     * 
     * @param aComponent
     *          the component to load the window preferences for;
     * @param aNamespace
     *          the namespace of the component to load the preferences for.
     */
    private void loadPreferences( final Window aComponent, final String aNamespace )
    {
      try
      {
        if ( aComponent instanceof Configurable )
        {
          this.logger.log( LogService.LOG_DEBUG, "Reading dialog-specific properties for: " + aNamespace );

          try
          {
            final UserSettings userSettings = getUserSettings( aNamespace );
            ( ( Configurable )aComponent ).readPreferences( userSettings );
          }
          catch ( Exception exception )
          {
            this.logger.log( LogService.LOG_DEBUG, "Failed to read preferences for: " + aNamespace, exception );
          }
        }

        // Only store settings of "real" frames and dialogs, not
        // popups/dropdowns, etc...
        if ( isManagedWindow( aComponent ) )
        {
          this.logger.log( LogService.LOG_DEBUG, "Reading window-properties for: " + aNamespace );

          final Preferences componentPrefs = getUserPreferences( aNamespace );
          SwingComponentUtils.loadWindowState( componentPrefs, aComponent );
        }
      }
      finally
      {
        registerPreferencesLoaded( aNamespace );
      }
    }

    /**
     * Marks for the given namespace that its (Window) preferences are loaded.
     * 
     * @param aNamespace
     *          the namespace to register as being loaded.
     */
    private void registerPreferencesLoaded( final String aNamespace )
    {
      this.prefsLoaded.put( aNamespace, Boolean.TRUE );
    }

    /**
     * Removes the flag that the preferences are loaded for the given namespace.
     * 
     * @param aNamespace
     *          the namespace key to check.
     */
    private void registerPreferencesSaved( final String aNamespace )
    {
      this.prefsLoaded.put( aNamespace, Boolean.FALSE );
    }

    /**
     * Saves the preferences for the given window using the given namespace.
     * 
     * @param aComponent
     *          the component to save the window preferences for;
     * @param aNamespace
     *          the namespace of the component to save the preferences for.
     */
    private void savePreferences( final Window aComponent, final String aNamespace )
    {
      try
      {
        if ( aComponent instanceof Configurable )
        {
          this.logger.log( LogService.LOG_DEBUG, "Writing dialog-specific properties for: " + aNamespace );

          try
          {
            final UserSettings userSettings = getUserSettings( aNamespace );
            ( ( Configurable )aComponent ).writePreferences( userSettings );
            // Important: write back the user settings to mark the project as
            // changed!
            setUserSettings( userSettings );
          }
          catch ( Exception exception )
          {
            this.logger.log( LogService.LOG_DEBUG, "Failed to safe properties for: " + aNamespace, exception );
          }
        }

        // Only store settings of "real" frames and dialogs, not
        // popups/dropdowns, etc...
        if ( isManagedWindow( aComponent ) )
        {
          this.logger.log( LogService.LOG_DEBUG, "Writing window-properties for: " + aNamespace );

          final Preferences componentPrefs = getUserPreferences( aNamespace );
          SwingComponentUtils.saveWindowState( componentPrefs, aComponent );
        }
      }
      catch ( RuntimeException exception )
      {
        this.logger.log( LogService.LOG_WARNING, "Writing dialog properties failed!", exception );
      }
      finally
      {
        registerPreferencesSaved( aNamespace );
      }
    }

    /**
     * Stores the given user settings in the current project.
     * 
     * @param aUserSettings
     *          the user settings for the given namespace, cannot be
     *          <code>null</code>.
     */
    private void setUserSettings( final UserSettings aUserSettings )
    {
      this.projectManager.getCurrentProject().setSettings( aUserSettings );
    }
  }

  // CONSTANTS

  /** The name of the implicit user settings properties file name. */
  private static final String IMPLICIT_USER_SETTING_NAME_PREFIX = "nl.lxtreme.ols.client";
  private static final String IMPLICIT_USER_SETTING_NAME_SUFFIX = "settings";

  // VARIABLES

  // All volatiles below are injected by Felix DM...
  private volatile DependencyManager dependencyManager;
  private volatile ProjectManager projectManager;
  private volatile UserSettingsManager userSettingsManager;
  private volatile LogService log;

  private Component windowStateComponent;

  // METHODS

  /**
   * Called by the dependency manager when all dependencies are satisfied, and
   * this component can be started.
   */
  public void start()
  {
    // Restore any implicit user settings from our previous session...
    loadImplicitUserSettings();

    this.windowStateComponent = this.dependencyManager.createComponent();
    this.windowStateComponent.setImplementation( new WindowStateListener() ) //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( ProjectManager.class ) //
            .setRequired( true ) //
        ) //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( PreferencesService.class ) //
            .setRequired( true ) //
        ) //
        .add( this.dependencyManager.createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) //
        );

    this.dependencyManager.add( this.windowStateComponent );
  }

  /**
   * Called by the dependency manager when it is shutting down this component.
   */
  public void stop()
  {
    // Store all implicit user settings for our next session...
    saveImplicitUserSettings();

    this.dependencyManager.remove( this.windowStateComponent );
  }

  /**
   * Creates an OS-specific file location to store data.
   * 
   * @param aName
   *          the name of the data file, excluding the file extension, cannot be
   *          <code>null</code> or empty;
   * @param aExtension
   *          the extension of the data file to use, note that this is an
   *          <em>indication</em> an might not be used for a particular host
   *          operating system.
   * @return the file pointing to the OS-specific properties file location,
   *         never <code>null</code>.
   */
  private File createLocalDataFile( final String aName, final String aExtension )
  {
    final String fileName;
    final String extension = ( aExtension.startsWith( "." ) ? "" : "." ) + aExtension;

    String dirName;
    if ( isMacOS() )
    {
      // This is the location where to store data on MacOS...
      dirName = System.getProperty( "user.home" ) + "/Library/Preferences";
      fileName = aName + ".Application";
    }
    else if ( isUnix() )
    {
      // The home folder is the 'default' location on Unix flavors...
      dirName = System.getProperty( "user.home" );
      fileName = "." + aName + extension;
    }
    else
    {
      // On Windows, there's no 'single' concept where to store local
      // application data...
      dirName = System.getenv( "LOCALAPPDATA" );
      if ( ( dirName == null ) || dirName.trim().isEmpty() )
      {
        System.getenv( "APPDATA" );
      }
      if ( ( dirName == null ) || dirName.trim().isEmpty() )
      {
        dirName = System.getProperty( "user.home" );
      }

      fileName = aName + extension;
    }

    final File propFile = new File( dirName, fileName );
    return propFile;
  }

  /**
   * Returns the file-object pointing to a user-settings file.
   * 
   * @return a user-settings file-object, never <code>null</code>.
   */
  private File getUserSettingsFile()
  {
    return createLocalDataFile( IMPLICIT_USER_SETTING_NAME_PREFIX, IMPLICIT_USER_SETTING_NAME_SUFFIX );
  }

  /**
   * Returns whether the current host's operating system is Linux or any other
   * UNIX-like operating system, such as Solaris (SunOS).
   * 
   * @return <code>true</code> if running on Linux or any other UNIX system,
   *         <code>false</code> otherwise.
   */
  private boolean isLinux()
  {
    String osName = System.getProperty( "os.name" ).toLowerCase();
    return ( osName.indexOf( "linux" ) >= 0 );
  }

  /**
   * Returns whether the current host's operating system is Mac OS X.
   * 
   * @return <code>true</code> if running on Mac OS X, <code>false</code>
   *         otherwise.
   */
  private boolean isMacOS()
  {
    final String osName = System.getProperty( "os.name" );
    return ( "Mac OS X".equalsIgnoreCase( osName ) || "Darwin".equalsIgnoreCase( osName ) );
  }

  /**
   * Returns whether the current host's operating system is Sun/Open Solaris.
   * 
   * @return <code>true</code> if running on Sun/Open Solaris system,
   *         <code>false</code> otherwise.
   */
  private boolean isSolaris()
  {
    String osName = System.getProperty( "os.name" ).toLowerCase();
    return ( osName.indexOf( "solaris" ) >= 0 ) || //
        ( osName.indexOf( "sunos" ) >= 0 );
  }

  /**
   * Returns whether the current host's operating system is Linux or any other
   * UNIX-like operating system, such as Solaris (SunOS).
   * 
   * @return <code>true</code> if running on Linux or any other UNIX system,
   *         <code>false</code> otherwise.
   */
  private boolean isUnix()
  {
    String osName = System.getProperty( "os.name" ).toLowerCase();
    return ( osName.indexOf( "nix" ) >= 0 ) || isLinux() || isSolaris();
  }

  /**
   * Loads the implicit user settings for the given project manager.
   */
  private void loadImplicitUserSettings()
  {
    final File userSettingsFile = getUserSettingsFile();
    final Project currentProject = this.projectManager.getCurrentProject();
    try
    {
      this.userSettingsManager.loadUserSettings( userSettingsFile, currentProject );

      this.log.log( LogService.LOG_DEBUG, "User settings restored ..." );
    }
    finally
    {
      currentProject.setChanged( false );
    }
  }

  /**
   * Saves the implicit user settings for the given project manager.
   */
  private void saveImplicitUserSettings()
  {
    final Project currentProject = this.projectManager.getCurrentProject();
    if ( currentProject.isChanged() )
    {
      try
      {
        final File userSettingsFile = getUserSettingsFile();
        this.userSettingsManager.saveUserSettings( userSettingsFile, currentProject );

        this.log.log( LogService.LOG_DEBUG, "User settings stored ..." );
      }
      finally
      {
        currentProject.setChanged( false );
      }
    }
  }
}
