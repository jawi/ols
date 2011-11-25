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
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.client.data.settings.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;

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
  static final class WindowStateListener implements AWTEventListener
  {
    // CONSTANTS

    private static final Logger LOG = Logger.getLogger( WindowStateListener.class.getName() );

    // VARIABLES

    private final Map<String, Boolean> prefsLoaded;
    private final PreferencesService preferenceService;
    private final ProjectManager projectManager;
    private final String userName;

    // CONSTRUCTORS

    /**
     * Creates a new FrameStateListener instance.
     * 
     * @param aPreferences
     *          the preferences to pass to the individual opened windows;
     * @param aProjectManager
     *          the project manager that provides the (current) user settings;
     * @param aUserName
     *          the name of the user running the client. Used to "partition" the
     *          window preferences for individual users in global preferences
     *          storage.
     */
    public WindowStateListener( final PreferencesService aPreferenceService, final ProjectManager aProjectManager,
        final String aUserName )
    {
      this.userName = aUserName;
      this.projectManager = aProjectManager;
      this.preferenceService = aPreferenceService;

      this.prefsLoaded = new ConcurrentHashMap<String, Boolean>();
    }

    // METHODS

    /**
     * @see java.awt.event.AWTEventListener#eventDispatched(java.awt.AWTEvent)
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      if ( !( aEvent instanceof ComponentEvent ) )
      {
        return;
      }

      final ComponentEvent event = ( ComponentEvent )aEvent;
      final Window component = ( Window )event.getComponent();
      final String namespace = component.getClass().getName();
      final int id = aEvent.getID();

      if ( ( id == WindowEvent.WINDOW_OPENED ) || ( id == WindowEvent.WINDOW_ACTIVATED ) )
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
          LOG.log( Level.FINE, "Reading dialog-specific properties for {0} ...", aNamespace );

          final UserSettings userSettings = getUserSettings( aNamespace );
          ( ( Configurable )aComponent ).readPreferences( userSettings );
        }

        // Only store settings of "real" frames and dialogs, not
        // popups/dropdowns, etc...
        if ( ( aComponent instanceof JFrame ) || ( aComponent instanceof JDialog ) )
        {
          LOG.log( Level.FINE, "Reading window-properties for {0} ...", aNamespace );

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
          LOG.log( Level.FINE, "Writing dialog-specific properties for {0} ...", aNamespace );

          final UserSettings userSettings = getUserSettings( aNamespace );
          ( ( Configurable )aComponent ).writePreferences( userSettings );
          // Important: write back the user settings to mark the project as
          // changed!
          setUserSettings( userSettings );
        }

        // Only store settings of "real" frames and dialogs, not
        // popups/dropdowns, etc...
        if ( ( aComponent instanceof JFrame ) || ( aComponent instanceof JDialog ) )
        {
          LOG.log( Level.FINE, "Writing window-properties for {0} ...", aNamespace );

          final Preferences componentPrefs = getUserPreferences( aNamespace );
          SwingComponentUtils.saveWindowState( componentPrefs, aComponent );
        }
      }
      catch ( RuntimeException exception )
      {
        LOG.log( Level.WARNING, "Writing dialog properties failed!", exception );
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

  private static final Logger LOG = Logger.getLogger( UserSessionManager.class.getName() );

  // VARIABLES

  private volatile ProjectManager projectManager;
  private volatile PreferencesService preferenceService = null;
  private WindowStateListener windowStateListener = null;

  // METHODS

  /**
   * Called by the dependency manager when the preference service becomes
   * available.
   * 
   * @param aPreferenceService
   */
  public void setPreferenceService( final PreferencesService aPreferenceService )
  {
    this.preferenceService = aPreferenceService;
  }

  /**
   * Called by the dependency manager when the project manager service becomes
   * available.
   * 
   * @param aProjectManager
   */
  public void setProjectManager( final ProjectManager aProjectManager )
  {
    this.projectManager = aProjectManager;
  }

  /**
   * Called by the dependency manager when all dependencies are satisfied, and
   * this component can be started.
   */
  public void start()
  {
    // Restore any implicit user settings from our previous session...
    loadImplicitUserSettings();

    if ( this.windowStateListener == null )
    {
      final String userName = System.getProperty( "user.name", "default" );

      this.windowStateListener = new WindowStateListener( this.preferenceService, this.projectManager, userName );
      // Install a global window state listener...
      Toolkit.getDefaultToolkit().addAWTEventListener( this.windowStateListener, AWTEvent.WINDOW_EVENT_MASK );

      LOG.fine( "AWT Window state listener installed..." );
    }
  }

  /**
   * Called by the dependency manager when it is shutting down this component.
   */
  public void stop()
  {
    // Store all implicit user settings for our next session...
    saveImplicitUserSettings();

    if ( this.windowStateListener != null )
    {
      Toolkit.getDefaultToolkit().removeAWTEventListener( this.windowStateListener );

      this.windowStateListener = null;

      LOG.fine( "AWT Window state listener removed..." );
    }
  }

  /**
   * Returns the file-object pointing to a user-settings file.
   * 
   * @return a user-settings file-object, never <code>null</code>.
   */
  private File getUserSettingsFile()
  {
    return HostUtils.createLocalDataFile( IMPLICIT_USER_SETTING_NAME_PREFIX, IMPLICIT_USER_SETTING_NAME_SUFFIX );
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
      UserSettingsManager.loadUserSettings( userSettingsFile, currentProject );

      LOG.fine( "Implicit user settings restored ..." );
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
        UserSettingsManager.saveUserSettings( userSettingsFile, currentProject );

        LOG.fine( "Implicit user settings stored ..." );
      }
      finally
      {
        currentProject.setChanged( false );
      }
    }
  }
}
