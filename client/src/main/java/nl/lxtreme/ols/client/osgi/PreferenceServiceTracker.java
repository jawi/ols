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
package nl.lxtreme.ols.client.osgi;


import java.awt.*;
import java.awt.event.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;
import org.osgi.service.prefs.*;
import org.osgi.util.tracker.*;


/**
 * @author jawi
 */
public class PreferenceServiceTracker extends ServiceTracker
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

    private final PreferencesService preferenceService;
    private final ProjectManager projectManager;
    private final String userName;

    // CONSTRUCTORS

    /**
     * Creates a new FrameStateListener instance.
     * 
     * @param aPreferences
     *          the preferences to pass to the individual opened windows.
     */
    public WindowStateListener( final PreferencesService aPreferenceService, final ProjectManager aProjectManager,
        final String aUserName )
    {
      this.userName = aUserName;
      this.projectManager = aProjectManager;
      this.preferenceService = aPreferenceService;
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
        // When we've already set the preferences once; don't do this again...
        if ( arePreferencesLoaded( namespace ) )
        {
          return;
        }

        try
        {
          if ( component instanceof Configurable )
          {
            LOG.log( Level.FINE, "Reading dialog-specific properties for {0} ...", namespace );

            final UserSettings userSettings = getUserSettings( namespace );
            ( ( Configurable )component ).readPreferences( userSettings );
          }

          // Only store settings of "real" frames and dialogs, not
          // popups/dropdowns, etc...
          if ( ( component instanceof JFrame ) || ( component instanceof JDialog ) )
          {
            LOG.log( Level.FINE, "Reading window-properties for {0} ...", namespace );

            final Preferences componentPrefs = getUserPreferences( namespace );
            SwingComponentUtils.loadWindowState( componentPrefs, component );
          }
        }
        catch ( RuntimeException exception )
        {
          LOG.log( Level.WARNING, "Reading dialog properties failed!", exception );
        }
        finally
        {
          registerPreferencesLoaded( namespace );
        }
      }
      else if ( id == WindowEvent.WINDOW_CLOSED )
      {
        try
        {
          if ( component instanceof Configurable )
          {
            LOG.log( Level.FINE, "Writing dialog-specific properties for {0} ...", namespace );

            final UserSettings userSettings = getUserSettings( namespace );
            ( ( Configurable )component ).writePreferences( userSettings );
          }

          // Only store settings of "real" frames and dialogs, not
          // popups/dropdowns, etc...
          if ( ( component instanceof JFrame ) || ( component instanceof JDialog ) )
          {
            LOG.log( Level.FINE, "Writing window-properties for {0} ...", namespace );

            final Preferences componentPrefs = getUserPreferences( namespace );
            SwingComponentUtils.saveWindowState( componentPrefs, component );
          }
        }
        catch ( RuntimeException exception )
        {
          LOG.log( Level.WARNING, "Writing dialog properties failed!", exception );
        }
      }
    }

    /**
     * @param aNamespace
     * @return
     */
    private boolean arePreferencesLoaded( final String aNamespace )
    {
      final Preferences systemPreferences = this.preferenceService.getSystemPreferences();

      final Preferences windowPrefs = systemPreferences.node( PreferenceServiceTracker.OLS_WINDOW_PREFERENCES_KEY );
      return windowPrefs.getBoolean( aNamespace, false );
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
     * Returns the preferences node for the given namespace.
     * 
     * @param aNamespace
     *          the node namespace to retrieve the preferences for, cannot be
     *          <code>null</code>.
     * @return the preferences node for the given namespace, never
     *         <code>null</code>.
     */
    private UserSettings getUserSettings( final String aNamespace )
    {
      final UserSettings result = this.projectManager.getCurrentProject().getSettings( aNamespace );
      return result;
    }

    /**
     * @param aNamespace
     */
    private void registerPreferencesLoaded( final String aNamespace )
    {
      final Preferences systemPreferences = this.preferenceService.getSystemPreferences();
      final Preferences windowPrefs = systemPreferences.node( PreferenceServiceTracker.OLS_WINDOW_PREFERENCES_KEY );
      windowPrefs.putBoolean( aNamespace, true );
    }
  }

  // CONSTANTS

  public static final String OLS_WINDOW_PREFERENCES_KEY = "ols/windowPreferencesSet";

  // VARIABLES

  private final ProjectManager projectManager;
  private volatile PreferencesService preferenceService = null;
  private transient WindowStateListener windowStateListener = null;

  // CONSTRUCTORS

  /**
   * Creates a new PreferenceServiceTracker instance.
   * 
   * @param aContext
   *          the bundle context to use;
   * @param aController
   *          the host to use.
   */
  public PreferenceServiceTracker( final BundleContext aContext, final ProjectManager aProjectManager )
  {
    super( aContext, PreferencesService.class.getName(), null );

    this.projectManager = aProjectManager;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    this.preferenceService = ( PreferencesService )this.context.getService( aReference );
    final String userName = System.getProperty( "user.name", "default" );

    if ( this.windowStateListener == null )
    {
      this.windowStateListener = new WindowStateListener( this.preferenceService, this.projectManager, userName );
      // Install a global window state listener...
      Toolkit.getDefaultToolkit().addAWTEventListener( this.windowStateListener, AWTEvent.WINDOW_EVENT_MASK );
    }

    final Preferences systemPreferences = this.preferenceService.getSystemPreferences();

    // Clear any stored window preference keys...
    clearWindowPreferencesNode( systemPreferences );

    return this.preferenceService;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    if ( this.windowStateListener != null )
    {
      Toolkit.getDefaultToolkit().removeAWTEventListener( this.windowStateListener );

      this.windowStateListener = null;
    }

    try
    {
      this.context.ungetService( aReference );
    }
    catch ( IllegalStateException exception )
    {
      Logger.getLogger( getClass().getName() ).log( Level.FINE,
          "Ungetting service failed! Bundle context no longer valid!" );
    }
  }

  /**
   * Clears the preference-node in which the administration is kept which
   * windows have already their preferences set.
   */
  private void clearWindowPreferencesNode( final Preferences aSystemPreferences )
  {
    try
    {
      if ( aSystemPreferences.nodeExists( PreferenceServiceTracker.OLS_WINDOW_PREFERENCES_KEY ) )
      {
        aSystemPreferences.node( PreferenceServiceTracker.OLS_WINDOW_PREFERENCES_KEY ).removeNode();
        aSystemPreferences.flush();
      }
    }
    catch ( BackingStoreException exception )
    {
      // Ignore...
    }
  }
}
