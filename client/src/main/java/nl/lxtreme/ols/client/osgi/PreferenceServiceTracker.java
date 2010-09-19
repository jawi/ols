/**
 * 
 */
package nl.lxtreme.ols.client.osgi;


import java.awt.*;
import java.awt.event.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.client.*;
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
    private final String userName;

    // CONSTRUCTORS

    /**
     * Creates a new FrameStateListener instance.
     * 
     * @param aPreferences
     *          the preferences to pass to the individual opened windows.
     */
    public WindowStateListener( final PreferencesService aPreferenceService, final String aUserName )
    {
      this.userName = aUserName;
      this.preferenceService = aPreferenceService;
    }

    // METHODS

    /**
     * @see java.awt.event.AWTEventListener#eventDispatched(java.awt.AWTEvent)
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      final int id = aEvent.getID();
      if ( ( id == WindowEvent.WINDOW_OPENED ) && ( aEvent instanceof ComponentEvent ) )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Window component = ( Window )event.getComponent();

        try
        {
          final String namespace = component.getClass().getName();
          if ( component instanceof Configurable )
          {
            LOG.log( Level.FINE, "Reading dialog-specific properties for '{0}' ...", namespace );

            final Preferences componentPrefs = getPreferences( namespace );
            ( ( Configurable )component ).readPreferences( componentPrefs );
          }

          // Only store settings of "real" frames and dialogs, not
          // popups/dropdowns, etc...
          if ( ( component instanceof JFrame ) || ( component instanceof JDialog ) )
          {
            LOG.log( Level.FINE, "Reading window-properties for '{0}' ...", namespace );

            final Preferences componentPrefs = getPreferences( namespace );
            SwingComponentUtils.loadWindowState( componentPrefs, component );
          }
        }
        catch ( RuntimeException exception )
        {
          LOG.log( Level.WARNING, "Reading dialog properties failed!", exception );
        }
      }
      else if ( ( id == WindowEvent.WINDOW_CLOSED ) && ( aEvent instanceof ComponentEvent ) )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Window component = ( Window )event.getComponent();

        try
        {
          final String namespace = component.getClass().getName();
          if ( component instanceof Configurable )
          {
            LOG.log( Level.FINE, "Writing dialog-specific properties for '{0}' ...", namespace );

            final Preferences componentPrefs = getPreferences( namespace );
            ( ( Configurable )component ).writePreferences( componentPrefs );
          }

          // Only store settings of "real" frames and dialogs, not
          // popups/dropdowns, etc...
          if ( ( component instanceof JFrame ) || ( component instanceof JDialog ) )
          {
            LOG.log( Level.FINE, "Writing window-properties for '{0}' ...", namespace );

            final Preferences componentPrefs = getPreferences( namespace );
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
     * Returns the preferences node for the given namespace.
     * 
     * @param aNamespace
     *          the node namespace to retrieve the preferences for, cannot be
     *          <code>null</code>.
     * @return the preferences node for the given namespace, never
     *         <code>null</code>.
     */
    private Preferences getPreferences( final String aNamespace )
    {
      final Preferences userPreferences = this.preferenceService.getUserPreferences( this.userName );
      return userPreferences.node( aNamespace );
    }
  }

  // VARIABLES

  private final ClientController controller;

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
  public PreferenceServiceTracker( final BundleContext aContext, final ClientController aController )
  {
    super( aContext, PreferencesService.class.getName(), null );

    this.controller = aController;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final PreferencesService preferenceService = ( PreferencesService )this.context.getService( aReference );
    final String userName = System.getProperty( "user.name", "default" );

    if ( this.windowStateListener == null )
    {
      this.windowStateListener = new WindowStateListener( preferenceService, userName );
      // Install a global window state listener...
      Toolkit.getDefaultToolkit().addAWTEventListener( this.windowStateListener, AWTEvent.WINDOW_EVENT_MASK );
    }

    // Publish the current user preferences to the controller...
    this.controller.setPreferences( preferenceService.getUserPreferences( userName ) );

    return preferenceService;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    // Make sure the preferences are actually removed from the controller as
    // well...
    this.controller.setPreferences( null );

    if ( this.windowStateListener != null )
    {
      Toolkit.getDefaultToolkit().removeAWTEventListener( this.windowStateListener );

      this.windowStateListener = null;
    }
  }
}
