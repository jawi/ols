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
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.Configurable;
import nl.lxtreme.ols.client.osgi.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.HostUtils.ApplicationCallback;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;


/**
 * Denotes the client host.
 */
public final class Host implements ApplicationCallback
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

    private final Properties properties;

    // CONSTRUCTORS

    /**
     * Creates a new FrameStateListener instance.
     * 
     * @param aProperties
     *          the properties to pass to the individual opened windows.
     */
    public WindowStateListener( final Properties aProperties )
    {
      this.properties = aProperties;
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
            ( ( Configurable )component ).readProperties( namespace, this.properties );
          }

          // Only store settings of "real" frames and dialogs, not
          // popups/dropdowns, etc...
          if ( ( component instanceof JFrame ) || ( component instanceof JDialog ) )
          {
            LOG.log( Level.FINE, "Reading window-properties for '{0}' ...", namespace );
            SwingComponentUtils.loadWindowState( namespace, this.properties, component );
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
            ( ( Configurable )component ).writeProperties( namespace, this.properties );
          }

          // Only store settings of "real" frames and dialogs, not
          // popups/dropdowns, etc...
          if ( ( component instanceof JFrame ) || ( component instanceof JDialog ) )
          {
            LOG.log( Level.FINE, "Writing window-properties for '{0}' ...", namespace );
            SwingComponentUtils.saveWindowState( namespace, this.properties, component );
          }
        }
        catch ( RuntimeException exception )
        {
          LOG.log( Level.WARNING, "Writing dialog properties failed!", exception );
        }
      }
    }
  }

  // CONSTANT

  private static final Logger LOG = Logger.getLogger( Host.class.getName() );

  private static final String PROPERTIES_NAME = "nl.lxtreme.ols.client";

  public static final String SHORT_NAME = "LogicSniffer";
  public static final String FULL_NAME = SHORT_NAME.concat( " - Logic Analyzer Client" );

  // VARIABLES

  private final BundleContext context;
  private final Properties clientProperties;

  private MenuTracker menuTracker;
  private DeviceControllerTracker deviceControllerTracker;
  private ToolTracker toolTracker;
  private Properties userProperties;
  private ClientController controller;

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
      // Stop the framework bundle; which should stop all other bundles as
      // well...
      this.context.getBundle( 0 ).stop();
    }
    catch ( BundleException be )
    {
      System.exit( -1 );
    }
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
    return false;
  }

  /**
   * @see nl.lxtreme.ols.util.HostUtils.ApplicationCallback#handleQuit()
   */
  @Override
  public boolean handleQuit()
  {
    exit();
    // On MacOSX, it appears that if we acknowledge this event, the system
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
    return false;
  }

  /**
   * Initializes this host application. May <em>not</em> be called from outside
   * the EDT.
   */
  public void initialize()
  {
    // Read back the user preferences from last time...
    this.userProperties = HostUtils.readProperties( PROPERTIES_NAME );
    if ( this.userProperties == null )
    {
      this.userProperties = new Properties();
    }

    if ( Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) ) )
    {
      // Install a custom repaint manager that detects whether Swing components
      // are created outside the EDT; if so, it will yield a stack trace to the
      // offending parts of the code...
      ThreadViolationDetectionRepaintManager.install();
    }

    // Install a global window state listener...
    Toolkit.getDefaultToolkit().addAWTEventListener( new WindowStateListener( this.userProperties ),
        AWTEvent.WINDOW_EVENT_MASK );

    this.controller = new ClientController( this.context, this );

    final MainFrame mainFrame = new MainFrame( this.controller );
    this.controller.setMainFrame( mainFrame );

    this.deviceControllerTracker = new DeviceControllerTracker( this.context, this.controller );
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
    // Write back the user properties for use next time...
    HostUtils.writeProperties( PROPERTIES_NAME, this.userProperties );

    LOG.log( Level.FINE, "{0} shutting down ...", SHORT_NAME );
  }

  /**
   * Starts this host by making the main frame visible, may <em>not</em> be
   * called from outside the EDT.
   */
  public void start()
  {
    this.deviceControllerTracker.open();
    this.toolTracker.open();
    this.menuTracker.open();

    final MainFrame mainFrame = this.controller.getMainFrame();
    if ( mainFrame != null )
    {
      mainFrame.setVisible( true );
    }

    LOG.log( Level.INFO, "{0} v{1} started ...", new Object[] { SHORT_NAME, getVersion() } );
  }

  /**
   * Stops this host by making the main frame invisible, may <em>not</em> be
   * called from outside the EDT.
   */
  public void stop()
  {
    final MainFrame mainFrame = this.controller.getMainFrame();
    if ( mainFrame != null )
    {
      mainFrame.dispose();
      this.controller.setMainFrame( null );
    }

    this.deviceControllerTracker.close();
    this.toolTracker.close();
    this.menuTracker.close();

    LOG.log( Level.INFO, "{0} stopped ...", SHORT_NAME );
  }
}

/* EOF */
