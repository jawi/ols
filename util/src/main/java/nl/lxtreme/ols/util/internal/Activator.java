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
package nl.lxtreme.ols.util.internal;


import java.awt.event.*;
import java.io.*;
import java.lang.reflect.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.osgi.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;


/**
 * Bundle activator.
 */
public class Activator implements BundleActivator
{
  // INNER TYPES

  /**
   * 
   */
  static class ApplicationCallbackServiceHelper implements ApplicationCallback, Closeable
  {
    // VARIABLES

    private final WhiteboardHelper<ApplicationCallback> serviceHelper;

    // CONSTRUCTORS

    /**
     * Creates a new Activator.ApplicationCallbackServiceHelper instance.
     */
    public ApplicationCallbackServiceHelper( final BundleContext aContext )
    {
      this.serviceHelper = new WhiteboardHelper<ApplicationCallback>( aContext, ApplicationCallback.class );
      this.serviceHelper.open();
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() throws IOException
    {
      this.serviceHelper.close();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean handleAbout()
    {
      final ApplicationCallback service = ( ApplicationCallback )this.serviceHelper.getService();
      if ( service != null )
      {
        return service.handleAbout();
      }
      return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean handlePreferences()
    {
      final ApplicationCallback service = ( ApplicationCallback )this.serviceHelper.getService();
      if ( service != null )
      {
        return service.handlePreferences();
      }
      return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean handleQuit()
    {
      final ApplicationCallback service = ( ApplicationCallback )this.serviceHelper.getService();
      if ( service != null )
      {
        return service.handleQuit();
      }
      return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasPreferences()
    {
      final ApplicationCallback service = ( ApplicationCallback )this.serviceHelper.getService();
      if ( service != null )
      {
        return service.hasPreferences();
      }
      return false;
    }
  }

  /**
   * Provides an {@link Action} for closing a {@link JOptionPane}.
   */
  static final class CloseOptionPaneAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    /**
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      final JOptionPane optionPane = ( JOptionPane )aEvent.getSource();
      optionPane.setValue( Integer.valueOf( JOptionPane.CLOSED_OPTION ) );
    }
  }

  /**
   * Provides a hack to ensure the system class loader is used at all times when
   * loading UI classes/resources/...
   */
  static class CLValue implements UIDefaults.ActiveValue
  {
    /**
     * @see javax.swing.UIDefaults.ActiveValue#createValue(javax.swing.UIDefaults)
     */
    public @Override
    ClassLoader createValue( final UIDefaults aDefaults )
    {
      return HostUtils.class.getClassLoader();
    }
  }

  /**
   * An OSX about handler.
   */
  static class OSXAboutHandler implements InvocationHandler
  {
    // CONSTANTS

    private static final String ABOUT_HANDLER_CLASS_NAME = "com.apple.eawt.AboutHandler";

    // VARIABLES

    private final ApplicationCallback callback;

    // CONSTRUCTORS

    /**
     * Creates a new HostUtils.OSXAboutHandler instance.
     */
    OSXAboutHandler( final ApplicationCallback aCallback )
    {
      this.callback = aCallback;
    }

    // METHODS

    /**
     * @param aCallback
     * @return
     * @throws ClassNotFoundException
     */
    public static Object createInstance( final ApplicationCallback aCallback ) throws ClassNotFoundException
    {
      final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      return Proxy
          .newProxyInstance( classLoader, new Class<?>[] { getHandlerClass() }, new OSXAboutHandler( aCallback ) );
    }

    /**
     * @return
     * @throws ClassNotFoundException
     */
    public static Class<?> getHandlerClass() throws ClassNotFoundException
    {
      final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      return classLoader.loadClass( ABOUT_HANDLER_CLASS_NAME );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      if ( "handleAbout".equals( aMethod.getName() ) )
      {
        this.callback.handleAbout();
      }
      return null;
    }
  }

  /**
   * An OSX preferences handler.
   */
  static class OSXPreferencesHandler implements InvocationHandler
  {
    // CONSTANTS

    private static final String PREFS_HANDLER_CLASS_NAME = "com.apple.eawt.PreferencesHandler";

    // VARIABLES

    private final ApplicationCallback callback;

    // CONSTRUCTORS

    /**
     * Creates a new HostUtils.OSXPreferencesHandler instance.
     */
    OSXPreferencesHandler( final ApplicationCallback aCallback )
    {
      this.callback = aCallback;
    }

    // METHODS

    /**
     * @param aCallback
     * @return
     * @throws ClassNotFoundException
     */
    public static Object createInstance( final ApplicationCallback aCallback ) throws ClassNotFoundException
    {
      final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      return Proxy.newProxyInstance( classLoader, new Class<?>[] { getHandlerClass() }, new OSXPreferencesHandler(
          aCallback ) );
    }

    /**
     * @return
     * @throws ClassNotFoundException
     */
    public static Class<?> getHandlerClass() throws ClassNotFoundException
    {
      final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      return classLoader.loadClass( PREFS_HANDLER_CLASS_NAME );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      if ( "handlePreferences".equals( aMethod.getName() ) )
      {
        this.callback.handlePreferences();
      }
      return null;
    }
  }

  /**
   * An OSX quit handler.
   */
  static class OSXQuitHandler implements InvocationHandler
  {
    // CONSTANTS

    private static final String QUIT_HANDLER_CLASS_NAME = "com.apple.eawt.QuitHandler";

    // VARIABLES

    private final ApplicationCallback callback;

    // CONSTRUCTORS

    /**
     * Creates a new HostUtils.OSXQuitHandler instance.
     */
    OSXQuitHandler( final ApplicationCallback aCallback )
    {
      this.callback = aCallback;
    }

    // METHODS

    /**
     * @param aCallback
     * @return
     * @throws ClassNotFoundException
     */
    public static Object createInstance( final ApplicationCallback aCallback ) throws ClassNotFoundException
    {
      final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      return Proxy
          .newProxyInstance( classLoader, new Class<?>[] { getHandlerClass() }, new OSXQuitHandler( aCallback ) );
    }

    /**
     * @return
     * @throws ClassNotFoundException
     */
    public static Class<?> getHandlerClass() throws ClassNotFoundException
    {
      final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      return classLoader.loadClass( QUIT_HANDLER_CLASS_NAME );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      if ( "handleQuitRequestWith".equals( aMethod.getName() ) )
      {
        final boolean confirmQuit = this.callback.handleQuit();

        if ( ( aArgs.length > 1 ) && ( aArgs[1] != null ) )
        {
          final Object quitResponseObj = aArgs[1];
          final Class<?> quitResponseClass = quitResponseObj.getClass();
          if ( "com.apple.eawt.QuitResponse".equals( quitResponseClass.getName() ) )
          {
            if ( confirmQuit )
            {
              final Method performQuitMethod = quitResponseClass.getMethod( "performQuit" );
              performQuitMethod.invoke( quitResponseObj );
            }
            else
            {
              final Method cancelQuitMethod = quitResponseClass.getMethod( "cancelQuit" );
              cancelQuitMethod.invoke( quitResponseObj );
            }
          }
        }
      }
      return null;
    }
  }

  // VARIABLES

  private ApplicationCallbackServiceHelper serviceHelper;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.serviceHelper = new ApplicationCallbackServiceHelper( aContext );

    initOSSpecifics( "TEST!", this.serviceHelper );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    this.serviceHelper.close();
  }

  /**
   * Initializes the OS-specific stuff.
   * 
   * @param aApplicationName
   *          the name of the application (when this needs to be passed to the
   *          guest OS);
   * @param aApplicationCallback
   *          the application callback used to report application events on some
   *          platforms (Mac OS), may be <code>null</code>.
   */
  private void initOSSpecifics( final String aApplicationName, final ApplicationCallback aApplicationCallback )
  {
    final HostInfo hostInfo = HostUtils.getHostInfo();
    if ( hostInfo.isMacOS() )
    {
      // Moves the main menu bar to the screen menu bar location...
      System.setProperty( "apple.laf.useScreenMenuBar", "true" );
      System.setProperty( "apple.awt.graphics.EnableQ2DX", "true" );
      System.setProperty( "com.apple.mrj.application.apple.menu.about.name", aApplicationName );
      System.setProperty( "com.apple.mrj.application.growbox.intrudes", "false" );
      System.setProperty( "com.apple.mrj.application.live-resize", "false" );
      System.setProperty( "com.apple.macos.smallTabs", "true" );
      System.setProperty( "apple.eawt.quitStrategy", "CLOSE_ALL_WINDOWS" );

      // Install an additional accelerator (Cmd+W) for closing option panes...
      ActionMap map = ( ActionMap )UIManager.get( "OptionPane.actionMap" );
      if ( map == null )
      {
        map = new ActionMap();
        UIManager.put( "OptionPane.actionMap", map );
      }
      map.put( "close", new CloseOptionPaneAction() );

      UIManager.put( "OptionPane.windowBindings", //
          new Object[] { SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_W ), "close", "ESCAPE", "close" } );

      if ( aApplicationCallback != null )
      {
        installApplicationCallback( aApplicationCallback );
      }
    }
    else if ( hostInfo.isUnix() )
    {
      try
      {
        UIManager.put( "Application.useSystemFontSettings", Boolean.FALSE );
        setLookAndFeel( "com.jgoodies.looks.plastic.Plastic3DLookAndFeel" );
      }
      catch ( Exception exception )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, "Failed to set look and feel!", exception );
      }
    }
    else if ( hostInfo.isWindows() )
    {
      try
      {
        UIManager.put( "Application.useSystemFontSettings", Boolean.TRUE );
        setLookAndFeel( "com.jgoodies.looks.plastic.PlasticXPLookAndFeel" );
      }
      catch ( Exception exception )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, "Failed to set look and feel!", exception );
      }
    }
  }

  /**
   * @param aApplicationCallback
   */
  private void installApplicationCallback( final ApplicationCallback aApplicationCallback )
  {
    final String applicationClassName = "com.apple.eawt.Application";

    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    try
    {
      final Object aboutHandler = OSXAboutHandler.createInstance( aApplicationCallback );
      final Object prefsHandler = OSXPreferencesHandler.createInstance( aApplicationCallback );
      final Object quitHandler = OSXQuitHandler.createInstance( aApplicationCallback );

      final Class<?> appClass = classLoader.loadClass( applicationClassName );

      if ( appClass != null )
      {
        // Call Application#getApplication() ...
        final Method getAppMethod = appClass.getMethod( "getApplication" );
        final Object app = getAppMethod.invoke( null );

        // Call Application#setAboutHandler() ...
        final Method setAboutHandlerMethod = appClass.getMethod( "setAboutHandler", OSXAboutHandler.getHandlerClass() );
        setAboutHandlerMethod.invoke( app, aboutHandler );

        // Call Application#setPreferencesHandler() ...
        final Method setPreferencesHandlerMethod = appClass.getMethod( "setPreferencesHandler",
            OSXPreferencesHandler.getHandlerClass() );
        setPreferencesHandlerMethod.invoke( app, prefsHandler );

        // Call Application#setQuitHandler() ...
        final Method setQuitHandlerMethod = appClass.getMethod( "setQuitHandler", OSXQuitHandler.getHandlerClass() );
        setQuitHandlerMethod.invoke( app, quitHandler );
      }
    }
    catch ( Exception exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        Logger.getAnonymousLogger().log( Level.ALL, "Install application callback failed!", exception );
      }
    }
  }

  /**
   * @param aLookAndFeelClass
   */
  private void setLookAndFeel( final String aLookAndFeelClassName )
  {
    final UIDefaults defaults = UIManager.getDefaults();
    // to make sure we always use system class loader
    defaults.put( "ClassLoader", new CLValue() );

    final ClassLoader oldCL = Thread.currentThread().getContextClassLoader();
    try
    {
      Thread.currentThread().setContextClassLoader( HostUtils.class.getClassLoader() );
      UIManager.setLookAndFeel( aLookAndFeelClassName );
    }
    catch ( Exception exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, "Failed to set look and feel!", exception );
      }
    }
    finally
    {
      Thread.currentThread().setContextClassLoader( oldCL );
    }
  }

}
