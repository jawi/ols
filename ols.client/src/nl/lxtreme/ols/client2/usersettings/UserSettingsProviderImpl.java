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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.usersettings;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.*;
import java.util.zip.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.service.log.*;
import org.osgi.service.prefs.*;


/**
 * Provides an implementation of {@link UserSettingProvider}.
 */
public class UserSettingsProviderImpl implements UserSettingProvider
{
  // INNER TYPES

  /**
   * Default implementation of {@link UserSettings}.
   */
  static class UserSettingsImpl implements UserSettings
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final String name;
    private final ConcurrentMap<String, String> properties;

    // CONSTRUCTORS

    /**
     * Creates a new {@link UserSettingsImpl} instance.
     */
    public UserSettingsImpl( String aName )
    {
      this.name = aName;
      this.properties = new ConcurrentHashMap<String, String>();
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Entry<String, String>> entrySet()
    {
      return this.properties.entrySet();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String get( String aName, String aDefaultValue )
    {
      Object result = this.properties.get( aName );
      if ( result == null || !( result instanceof String ) )
      {
        return aDefaultValue;
      }
      return ( String )result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getBoolean( String aName, boolean aDefaultValue )
    {
      String result = this.properties.get( aName );
      if ( result == null )
      {
        return aDefaultValue;
      }
      return Boolean.parseBoolean( result.toString() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getInt( String aName, int aDefaultValue )
    {
      String result = this.properties.get( aName );
      if ( result == null )
      {
        return aDefaultValue;
      }
      try
      {
        return Integer.parseInt( result.toString() );
      }
      catch ( NumberFormatException exception )
      {
        return aDefaultValue;
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getLong( String aName, long aDefaultValue )
    {
      String result = this.properties.get( aName );
      if ( result == null )
      {
        return aDefaultValue;
      }
      try
      {
        return Long.parseLong( result.toString() );
      }
      catch ( NumberFormatException exception )
      {
        return aDefaultValue;
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getName()
    {
      return this.name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void put( String aName, String aValue )
    {
      this.properties.put( aName, aValue );
    }

    /**
     * {@inheritDoc}
     */
    public void putAll( Properties aSettings )
    {
      for ( Object k : aSettings.keySet() )
      {
        String key = String.valueOf( k );
        String value = aSettings.getProperty( key );
        this.properties.put( key, value );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putBoolean( String aName, boolean aValue )
    {
      this.properties.put( aName, Boolean.toString( aValue ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putInt( String aName, int aValue )
    {
      this.properties.put( aName, Integer.toString( aValue ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putLong( String aName, long aValue )
    {
      this.properties.put( aName, Long.toString( aValue ) );
    }
  }

  /**
   * Defines a (global) AWT event listener for storing/retrieving the window
   * state.
   */
  final class WindowStateListener implements AWTEventListener
  {
    // VARIABLES

    private final ConcurrentMap<String, Boolean> prefsLoaded;
    // Injected by Felix DM...
    private volatile LogService logger;

    // CONSTRUCTORS

    /**
     * Creates a new {@link WindowStateListener} instance.
     */
    public WindowStateListener()
    {
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
     * Returns whether or not the given component is a managed window.
     * 
     * @param aComponent
     *          the component to test, can be <code>null</code>.
     * @return <code>false</code> if the given component is not a managed window
     *         (or <code>null</code>), <code>true</code> if it is.
     */
    private boolean isManagedWindow( final Window aComponent )
    {
      if ( aComponent == null )
      {
        return false;
      }
      return ( ( aComponent instanceof JFrame ) && !( ( JFrame )aComponent ).isUndecorated() )
          || ( ( aComponent instanceof JDialog ) && !( ( JDialog )aComponent ).isUndecorated() );
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
            UserSettings userSettings = getSettings( aNamespace );
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

          UserSettings componentPrefs = getSettings( aNamespace.concat( ".window" ) );
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
            UserSettings userSettings = getSettings( aNamespace );
            ( ( Configurable )aComponent ).writePreferences( userSettings );
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

          UserSettings componentPrefs = getSettings( aNamespace.concat( ".window" ) );
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
  }

  // CONSTANTS

  /** The name of the implicit user settings properties file name. */
  private static final String IMPLICIT_USER_SETTING_NAME_PREFIX = "nl.lxtreme.ols.client";
  private static final String IMPLICIT_USER_SETTING_NAME_SUFFIX = "settings";

  private static final String SETTINGS_ID_FILENAME = "settings.";

  // VARIABLES

  private final ConcurrentMap<String, UserSettingsImpl> settings;
  // All volatiles below are injected by Felix DM...
  private volatile DependencyManager dependencyManager;
  private volatile LogService log;

  private Component windowStateComponent;

  // CONSTRUCTORS

  /**
   * Creates a new {@link UserSettingsProviderImpl} instance.
   */
  public UserSettingsProviderImpl()
  {
    this.settings = new ConcurrentHashMap<String, UserSettingsImpl>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<UserSettings> getAllSettings()
  {
    return new ArrayList<UserSettings>( this.settings.values() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public UserSettings getSettings( String aName )
  {
    return internalGetSettings( aName );
  }

  /**
   * Called by the dependency manager when all dependencies are satisfied, and
   * this component can be started.
   */
  protected void start()
  {
    // Restore any implicit user settings from our previous session...
    loadImplicitUserSettings();

    this.windowStateComponent = this.dependencyManager.createComponent();
    this.windowStateComponent.setImplementation( new WindowStateListener() ) //
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
  protected void stop()
  {
    // Store all implicit user settings for our next session...
    saveImplicitUserSettings();

    this.dependencyManager.remove( this.windowStateComponent );
  }

  private void closeSilently( final Closeable aCloseable )
  {
    try
    {
      if ( aCloseable != null )
      {
        aCloseable.close();
      }
    }
    catch ( IOException e )
    {
      // Ignore...
    }
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
  private File createLocalDataFile( String aName, String aExtension )
  {
    String fileName;
    String extension = ( aExtension.startsWith( "." ) ? "" : "." ) + aExtension;
    String osName = System.getProperty( "os.name" ).toLowerCase();

    String dirName;
    if ( "mac os x".equals( osName ) || "darwin".equals( osName ) )
    {
      // This is the location where to store data on MacOS...
      dirName = System.getProperty( "user.home" ) + "/Library/Preferences";
      fileName = aName + ".Application";
    }
    else if ( osName.indexOf( "nix" ) >= 0 || osName.indexOf( "solaris" ) >= 0 || osName.indexOf( "sunos" ) >= 0
        || osName.indexOf( "linux" ) >= 0 || osName.indexOf( "bsd" ) >= 0 )
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
      if ( ( dirName == null ) || "".equals( dirName.trim() ) )
      {
        System.getenv( "APPDATA" );
      }
      if ( ( dirName == null ) || "".equals( dirName.trim() ) )
      {
        dirName = System.getProperty( "user.home" );
      }

      fileName = aName + extension;
    }

    return new File( dirName, fileName );
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
   * @param aName
   * @return the user settings implementation, never <code>null</code>.
   */
  private UserSettingsImpl internalGetSettings( String aName )
  {
    UserSettingsImpl settings = new UserSettingsImpl( aName );
    UserSettingsImpl result = this.settings.putIfAbsent( aName, settings );
    return ( result == null ) ? settings : result;
  }

  /**
   * Loads the implicit user settings for the given project manager.
   */
  private void loadImplicitUserSettings()
  {
    File file = getUserSettingsFile();
    if ( !file.exists() )
    {
      this.log.log( LogService.LOG_INFO, "Ignoring user settings from " + file + "; file does not exist..." );
      return;
    }

    this.log.log( LogService.LOG_INFO, "Loading user settings from " + file );

    InputStream is = null;
    ZipInputStream zipIS = null;

    try
    {
      is = new BufferedInputStream( new FileInputStream( file ) );
      zipIS = new ZipInputStream( is );

      ZipEntry ze = null;
      while ( ( ze = zipIS.getNextEntry() ) != null )
      {
        final String userSettingsName = ze.getName();
        // Ignore settings ID marker file...
        if ( userSettingsName.startsWith( SETTINGS_ID_FILENAME ) )
        {
          continue;
        }

        final Properties settings = new Properties();
        settings.load( zipIS );

        internalGetSettings( userSettingsName ).putAll( settings );

        zipIS.closeEntry();
      }
    }
    catch ( IOException exception )
    {
      this.log.log( LogService.LOG_WARNING, "Failed to load implicit user settings...", exception );
    }
    finally
    {
      closeSilently( zipIS );
      closeSilently( is );
    }
  }

  /**
   * Saves the implicit user settings for the given project manager.
   */
  private void saveImplicitUserSettings()
  {
    File file = getUserSettingsFile();

    this.log.log( LogService.LOG_INFO, "Saving user settings to " + file );

    OutputStream os = null;

    try
    {
      os = new BufferedOutputStream( new FileOutputStream( file ) );
      ZipOutputStream zipOS = new ZipOutputStream( os );

      for ( UserSettingsImpl settings : this.settings.values() )
      {
        ZipEntry zipEntry = new ZipEntry( settings.getName() );
        zipOS.putNextEntry( zipEntry );

        // Convert to a properties object...
        Properties props = new Properties();
        for ( Map.Entry<String, String> userSetting : settings.entrySet() )
        {
          props.put( userSetting.getKey(), userSetting.getValue() );
        }

        // Write the project settings
        props.store( zipOS, settings.getName().concat( " settings" ) );
      }

      closeSilently( zipOS );
    }
    catch ( IOException exception )
    {
      this.log.log( LogService.LOG_WARNING, "Failed to save implicit user settings...", exception );
    }
    finally
    {
      closeSilently( os );
    }
  }
}
