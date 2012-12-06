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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing;


import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;


/**
 * Provides a generic way of creating Swing windows (that is, {@link JDialog} or
 * {@link JFrame} instances) which have their state (re)stored.
 */
public class WindowManager
{
  // INNER TYPES

  /**
   * This interface defines the methods required to make (UI) object states
   * controllable by the project mechanism.
   * <p>
   * Note: when defining property values it should be kept in mind that the
   * project configuration file should be understandable and editable by users.
   * Use common sense to determine whether a particular setting should be part
   * of the project configuration or not. For key naming conventions please look
   * at an actual configuration file.
   * </p>
   */
  public static interface Configurable
  {
    // METHODS

    /**
     * Reads configuration from given preferences.
     * 
     * @param aSettings
     *          the user settings to read the configuration from, cannot be
     *          <code>null</code>.
     */
    void readPreferences( final UserSettings aSettings );

    /**
     * Writes configuration to given preferences.
     * 
     * @param aSettings
     *          the user settings to write the configuration to, cannot be
     *          <code>null</code>.
     */
    void writePreferences( final UserSettings aSettings );
  }

  /**
   * Denotes an abstract way for obtaining/storing user settings.
   */
  public static interface UserSettings
  {
    // METHODS

    /**
     * @return this user settings as {@link Properties} object, never
     *         <code>null</code>.
     */
    Properties asProperties();

    /**
     * Returns the string value associated with the given name, returning a
     * given default value if no value is (yet) associated.
     * 
     * @param aName
     *          the name of whose associated value should be returned;
     * @param aDefaultValue
     *          the default value to return.
     * @return the value associated with the given name, or the given default.
     */
    String get( String aName, String aDefaultValue );

    /**
     * Returns the boolean value associated with the given name, returning a
     * given default value if no value is (yet) associated.
     * 
     * @param aName
     *          the name of whose associated value should be returned;
     * @param aDefaultValue
     *          the default value to return.
     * @return the value associated with the given name, or the given default.
     */
    boolean getBoolean( String aName, boolean aDefaultValue );

    /**
     * Returns the integer value associated with the given name, returning a
     * given default value if no value is (yet) associated.
     * 
     * @param aName
     *          the name of whose associated value should be returned;
     * @param aDefaultValue
     *          the default value to return.
     * @return the value associated with the given name, or the given default.
     */
    int getInt( String aName, int aDefaultValue );

    /**
     * Returns the long value associated with the given name, returning a given
     * default value if no value is (yet) associated.
     * 
     * @param aName
     *          the name of whose associated value should be returned;
     * @param aDefaultValue
     *          the default value to return.
     * @return the value associated with the given name, or the given default.
     */
    long getLong( String aName, long aDefaultValue );

    /**
     * Returns the name of these user settings.
     * 
     * @return the name of these settings, never <code>null</code>.
     */
    String getName();

    /**
     * Associates the given String value to the given name.
     * 
     * @param aName
     *          the name to associate the value to;
     * @param aValue
     *          the value to associate to the given name.
     */
    void put( String aName, String aValue );

    /**
     * Associates the given boolean value to the given name.
     * 
     * @param aName
     *          the name to associate the value to;
     * @param aValue
     *          the value to associate to the given name.
     */
    void putBoolean( String aName, boolean aValue );

    /**
     * Associates the given integer value to the given name.
     * 
     * @param aName
     *          the name to associate the value to;
     * @param aValue
     *          the value to associate to the given name.
     */
    void putInt( String aName, int aValue );

    /**
     * Associates the given long value to the given name.
     * 
     * @param aName
     *          the name to associate the value to;
     * @param aValue
     *          the value to associate to the given name.
     */
    void putLong( String aName, long aValue );
  }

  /**
   * Provides a callback for listening to the window state.
   */
  public static interface WindowCloseCallback
  {
    // METHODS

    /**
     * Called when the window is closed.
     * 
     * @param aWindow
     *          the window that was closed, never <code>null</code>.
     */
    void windowClosed( Window aWindow );
  }

  /**
   * Provides an abstraction for managing the state of a window.
   * <p>
   * The state for a window is defined as:
   * </p>
   * <ol>
   * <li>Its dimensions (width and height, in pixels);</li>
   * <li>its position on screen (X,Y-coordinates, in pixels), and;</li>
   * <li>optionally, the state of its (sub)components.</li>
   * </ol>
   */
  public static interface WindowStateManager
  {
    // METHODS

    /**
     * Returns all bounds known to this manager.
     * 
     * @return an array of bounds, never <code>null</code>.
     */
    Map.Entry<String, Rectangle>[] getAllBounds();

    /**
     * Return all settings known to this manager.
     * 
     * @return an array of settings, never <code>null</code>.
     */
    Map.Entry<String, Properties>[] getAllSettings();

    /**
     * Returns the bounds for the given key specifying its width, height and
     * X,Y-position on screen.
     * 
     * @param aKey
     *          the key to return the bounds for, cannot be <code>null</code>.
     * @return the bounds for the given key, or <code>null</code> if no bounds
     *         are found for the given key.
     */
    Rectangle getBounds( String aKey );

    /**
     * Returns the settings for the given specifying the (implicit) user
     * settings for a window.
     * 
     * @param aKey
     *          the key to return the settings for, cannot be <code>null</code>
     *          .
     * @return the settings for the given key, or <code>null</code> if no
     *         settings are found for the given key.
     */
    Properties getSettings( String aKey );

    /**
     * Stores the given bounds under the given key.
     * 
     * @param aKey
     *          the key under which to store the bounds for, cannot be
     *          <code>null</code>;
     * @param aBounds
     *          the bounds to store, cannot be <code>null</code>.
     * @see #getBounds(String)
     */
    void putBounds( String aKey, Rectangle aBounds );

    /**
     * Stores the given user settings under the given key.
     * 
     * @param aKey
     *          the key under which to store the settings, cannot be
     *          <code>null</code>;
     * @param aSettings
     *          the settings to store, cannot be <code>null</code>.
     * @see #getSettings(String)
     */
    void putSettings( String aKey, Properties aSettings );
  }

  /**
   * Provides a null-implementation of {@link UserSettings}.
   */
  private static class DefaultUserSettings implements UserSettings
  {
    // VARIABLES

    private final String name;
    private final Properties properties;

    // CONSTRUCTORS

    /**
     * Creates a new {@link DefaultUserSettings} instance.
     */
    public DefaultUserSettings( final String aName )
    {
      this( aName, new Properties() );
    }

    /**
     * Creates a new {@link DefaultUserSettings} instance.
     */
    public DefaultUserSettings( final String aName, final Properties aProperties )
    {
      this.name = aName;
      this.properties = new Properties( aProperties );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Properties asProperties()
    {
      return this.properties;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String get( final String aName, final String aDefaultValue )
    {
      return this.properties.getProperty( aName, aDefaultValue );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getBoolean( final String aName, final boolean aDefaultValue )
    {
      String result = this.properties.getProperty( aName );
      if ( result == null )
      {
        return aDefaultValue;
      }
      return Boolean.parseBoolean( result );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getInt( final String aName, final int aDefaultValue )
    {
      String result = this.properties.getProperty( aName );
      if ( result == null )
      {
        return aDefaultValue;
      }
      return Integer.parseInt( result );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getLong( final String aName, final long aDefaultValue )
    {
      String result = this.properties.getProperty( aName );
      if ( result == null )
      {
        return aDefaultValue;
      }
      return Long.parseLong( result );
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
    public void put( final String aName, final String aValue )
    {
      this.properties.put( aName, aValue );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putBoolean( final String aName, final boolean aValue )
    {
      this.properties.put( aName, Boolean.toString( aValue ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putInt( final String aName, final int aValue )
    {
      this.properties.put( aName, Integer.toString( aValue ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putLong( final String aName, final long aValue )
    {
      this.properties.put( aName, Long.toString( aValue ) );
    }
  }

  /**
   * Provides a default in-memory implementation of {@link WindowStateManager}.
   */
  private static class MemoryStateManager implements WindowStateManager
  {
    // VARIABLES

    private final Map<String, Rectangle> bounds = new HashMap<String, Rectangle>();
    private final Map<String, Properties> settings = new HashMap<String, Properties>();

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings( "unchecked" )
    public Map.Entry<String, Rectangle>[] getAllBounds()
    {
      Collection<Map.Entry<String, Rectangle>> entries = this.bounds.entrySet();
      return entries.toArray( new Map.Entry[entries.size()] );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings( "unchecked" )
    public Map.Entry<String, Properties>[] getAllSettings()
    {
      Collection<Map.Entry<String, Properties>> entries = this.settings.entrySet();
      return entries.toArray( new Map.Entry[entries.size()] );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Rectangle getBounds( final String aKey )
    {
      return this.bounds.get( aKey );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Properties getSettings( final String aKey )
    {
      Properties settings = this.settings.get( aKey );
      if ( settings == null )
      {
        settings = new Properties();
        this.settings.put( aKey, settings );
      }
      return settings;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putBounds( final String aKey, final Rectangle aBounds )
    {
      this.bounds.put( aKey, aBounds );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putSettings( final String aKey, final Properties aSettings )
    {
      this.settings.put( aKey, aSettings );
    }
  }

  /**
   * Implements a {@link WindowStateListener} to keep track of the state of
   * windows. In case it receives a "window closing" event, it will
   * automatically call the {@link WindowManager#close(Window)} method for that
   * window.
   */
  private class WindowTracker extends WindowAdapter
  {
    // VARIABLES

    private final WindowCloseCallback callback;

    /**
     * Creates a new {@link WindowTracker} instance.
     */
    public WindowTracker( final WindowCloseCallback aCallback )
    {
      this.callback = aCallback;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosed( final WindowEvent aEvent )
    {
      Window window = aEvent.getWindow();

      handleWindowCloseEvent( window );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosing( final WindowEvent aEvent )
    {
      Window window = aEvent.getWindow();

      handleWindowCloseEvent( window );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowStateChanged( final WindowEvent aEvent )
    {
      int id = aEvent.getID();
      if ( id == WindowEvent.WINDOW_CLOSING )
      {
        windowClosing( aEvent );
      }
      else if ( id == WindowEvent.WINDOW_CLOSED )
      {
        windowClosed( aEvent );
      }
    }

    /**
     * @param aWindow
     *          the window to handle, cannot be <code>null</code>.
     */
    private void handleWindowCloseEvent( final Window aWindow )
    {
      if ( this.callback != null )
      {
        this.callback.windowClosed( aWindow );
      }

      close( aWindow );
    }
  }

  // VARIABLES

  private final Map<Window, WindowTracker> trackerRegistry = new WeakHashMap<Window, WindowTracker>();

  private volatile WindowStateManager stateManager = new MemoryStateManager();

  // METHODS

  /**
   * Closes the given window by making it invisible and disposing its resources
   * in addition to storing its position and dimensions.
   * 
   * @param aWindow
   *          the window to close, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           if the given window was <code>null</code>.
   */
  public void close( final Window aWindow )
  {
    if ( aWindow == null )
    {
      throw new IllegalArgumentException( "Window cannot be null!" );
    }

    // Stop tracking this window...
    WindowTracker tracker = this.trackerRegistry.remove( aWindow );
    if ( tracker != null )
    {
      aWindow.removeWindowListener( tracker );
    }

    // Store the bounds for this window...
    storeBounds( aWindow );
    // Store the (optional) user settings...
    storeSettings( aWindow );

    aWindow.setVisible( false );
    aWindow.dispose();
  }

  /**
   * Returns the state manager of this window manager.
   * 
   * @return the state manager, never <code>null</code>.
   */
  public WindowStateManager getWindowStateManager()
  {
    return this.stateManager;
  }

  /**
   * Sets the window state manager.
   * 
   * @param aStateManager
   *          the state manager to set, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           if the given state manager was <code>null</code>.
   */
  public void setWindowStateManager( final WindowStateManager aStateManager )
  {
    if ( aStateManager == null )
    {
      throw new IllegalArgumentException( "StateManager cannot be null!" );
    }
    this.stateManager = aStateManager;
  }

  /**
   * Opens and shows the given window on screen, restoring its position and
   * dimensions.
   * <p>
   * Windows that are made visible through this method are automatically tracked
   * and cause the {@link #close(Window)} to be called automatically upon
   * closing of the window.
   * </p>
   * 
   * @param aWindow
   *          the window to open and show, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           if the given window was <code>null</code>.
   */
  public void show( final Window aWindow )
  {
    show( aWindow, null );
  }

  /**
   * Opens and shows the given window on screen, restoring its position and
   * dimensions.
   * <p>
   * Windows that are made visible through this method are automatically tracked
   * and cause the {@link #close(Window)} to be called automatically upon
   * closing of the window.
   * </p>
   * 
   * @param aWindow
   *          the window to open and show, cannot be <code>null</code>;
   * @param aCallback
   *          the callback to call when the dialog is closed, can be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           if the given window was <code>null</code>.
   */
  public void show( final Window aWindow, final WindowCloseCallback aCallback )
  {
    if ( aWindow == null )
    {
      throw new IllegalArgumentException( "Window cannot be null!" );
    }

    // Ensure the preferred sizes are set on the window and all its
    // sub-components...
    aWindow.pack();

    Rectangle bounds = retrieveBounds( aWindow );
    if ( bounds != null )
    {
      aWindow.setBounds( bounds );
    }

    UserSettings settings = retrieveSettings( aWindow );
    if ( ( settings != null ) && ( aWindow instanceof Configurable ) )
    {
      ( ( Configurable )aWindow ).readPreferences( settings );
    }

    WindowTracker tracker = new WindowTracker( aCallback );
    this.trackerRegistry.put( aWindow, tracker );

    // Keep track of what's happening to the window...
    aWindow.addWindowListener( tracker );

    // Show the window on screen...
    aWindow.setVisible( true );
  }

  /**
   * {@inheritDoc}
   */
  protected Rectangle retrieveBounds( final Window aWindow )
  {
    Rectangle bounds = null;
    if ( aWindow != null )
    {
      String key = createKey( aWindow );
      bounds = this.stateManager.getBounds( key );
    }
    return bounds;
  }

  /**
   * {@inheritDoc}
   */
  protected UserSettings retrieveSettings( final Window aWindow )
  {
    UserSettings settings = null;
    if ( ( aWindow != null ) && ( aWindow instanceof Configurable ) )
    {
      String key = createKey( aWindow );
      Properties props = this.stateManager.getSettings( key );
      settings = new DefaultUserSettings( key, props );
    }
    return settings;
  }

  /**
   * {@inheritDoc}
   */
  protected void storeBounds( final Window aWindow )
  {
    if ( aWindow != null )
    {
      String key = createKey( aWindow );
      Rectangle bounds = aWindow.getBounds();
      this.stateManager.putBounds( key, bounds );
    }
  }

  /**
   * {@inheritDoc}
   */
  protected void storeSettings( final Window aWindow )
  {
    if ( ( aWindow != null ) && ( aWindow instanceof Configurable ) )
    {
      String key = createKey( aWindow );

      UserSettings settings = new DefaultUserSettings( key );

      ( ( Configurable )aWindow ).writePreferences( settings );

      this.stateManager.putSettings( key, settings.asProperties() );
    }
  }

  /**
   * @param aWindow
   * @return
   */
  private String createKey( final Window aWindow )
  {
    StringBuilder sb = new StringBuilder();
    sb.append( aWindow.getClass().getName() );
    if ( aWindow.getName() != null )
    {
      sb.append( "." ).append( aWindow.getName() );
    }
    return sb.toString();
  }
}
