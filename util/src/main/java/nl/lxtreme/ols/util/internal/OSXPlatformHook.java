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


import java.io.*;
import java.lang.reflect.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;


/**
 * OSX-specific implementation for {@link PlatformCallback}.
 */
public final class OSXPlatformHook
{
  // INNER TYPES

  static class OSXAboutHandler implements InvocationHandler
  {
    private final PlatformCallback callback;

    OSXAboutHandler( final PlatformCallback callback )
    {
      this.callback = callback;
    }

    public static Object createInstance( final ClassLoader cl, final PlatformCallback callback,
        final Class<?> handlerClass )
    {
      if ( callback == null )
      {
        return null;
      }
      return Proxy.newProxyInstance( cl, new Class<?>[] { handlerClass }, new OSXAboutHandler( callback ) );
    }

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

  static class OSXAppReOpenedListenerHandler implements InvocationHandler
  {
    private final PlatformCallback callback;

    public OSXAppReOpenedListenerHandler( final PlatformCallback callback )
    {
      this.callback = callback;
    }

    // METHODS

    public static Object createInstance( final ClassLoader cl, final PlatformCallback callback,
        final Class<?>... handlerClasses )
    {
      if ( callback == null )
      {
        return null;
      }
      return Proxy.newProxyInstance( cl, handlerClasses, new OSXAppReOpenedListenerHandler( callback ) );
    }

    @Override
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      if ( "appReOpened".equals( aMethod.getName() ) )
      {
        this.callback.handleAppReOpened();
      }
      return null;
    }
  }

  static class OSXOpenFilesHandler implements InvocationHandler
  {
    private final PlatformCallback callback;

    OSXOpenFilesHandler( final PlatformCallback callback )
    {
      this.callback = callback;
    }

    public static Object createInstance( final ClassLoader cl, final PlatformCallback callback,
        final Class<?> handlerClass )
    {
      if ( callback == null )
      {
        return null;
      }
      return Proxy.newProxyInstance( cl, new Class<?>[] { handlerClass }, new OSXOpenFilesHandler( callback ) );
    }

    @Override
    @SuppressWarnings( "unchecked" )
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      if ( "openFiles".equals( aMethod.getName() ) )
      {
        if ( ( aArgs.length > 0 ) && ( aArgs[0] != null ) )
        {
          final Object eventObj = aArgs[0];
          final Class<?> eventObjectClass = eventObj.getClass();
          if ( OPEN_FILES_EVENT_CLASS_NAME.equals( eventObjectClass.getName() ) )
          {
            Method getFilesMethod = eventObjectClass.getMethod( "getFiles" );

            List<File> files = ( List<File> )getFilesMethod.invoke( eventObj );
            this.callback.handleOpenFiles( files );
          }
        }
      }

      return null;
    }
  }

  static class OSXPrefsHandler implements InvocationHandler
  {
    private final PlatformCallback callback;

    OSXPrefsHandler( final PlatformCallback callback )
    {
      this.callback = callback;
    }

    public static Object createInstance( final ClassLoader cl, final PlatformCallback callback,
        final Class<?> handlerClass )
    {
      if ( callback == null )
      {
        return null;
      }
      return Proxy.newProxyInstance( cl, new Class<?>[] { handlerClass }, new OSXPrefsHandler( callback ) );
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

  static class OSXQuitHandler implements InvocationHandler
  {
    private final PlatformCallback callback;

    OSXQuitHandler( final PlatformCallback callback )
    {
      this.callback = callback;
    }

    // METHODS

    public static Object createInstance( final ClassLoader cl, final PlatformCallback callback,
        final Class<?> handlerClass )
    {
      if ( callback == null )
      {
        return null;
      }
      return Proxy.newProxyInstance( cl, new Class<?>[] { handlerClass }, new OSXQuitHandler( callback ) );
    }

    @Override
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      if ( "handleQuitRequestWith".equals( aMethod.getName() ) )
      {
        final boolean confirmQuit = this.callback.shouldQuit();

        if ( ( aArgs.length > 1 ) && ( aArgs[1] != null ) )
        {
          final Object quitResponseObj = aArgs[1];
          final Class<?> quitResponseClass = quitResponseObj.getClass();
          if ( QUIT_RESPONSE_CLASS_NAME.equals( quitResponseClass.getName() ) )
          {
            if ( confirmQuit )
            {
              Method performQuitMethod = quitResponseClass.getMethod( "performQuit" );
              performQuitMethod.invoke( quitResponseObj );
            }
            else
            {
              Method cancelQuitMethod = quitResponseClass.getMethod( "cancelQuit" );
              cancelQuitMethod.invoke( quitResponseObj );
            }
          }
        }
      }
      return null;
    }
  }

  // CONSTANTS

  static final String APPLICATION_CLASS_NAME = "com.apple.eawt.Application";
  static final String ABOUT_HANDLER_CLASS_NAME = "com.apple.eawt.AboutHandler";
  static final String OPEN_FILES_HANDLER_CLASS_NAME = "com.apple.eawt.OpenFilesHandler";
  static final String PREFS_HANDLER_CLASS_NAME = "com.apple.eawt.PreferencesHandler";
  static final String QUIT_HANDLER_CLASS_NAME = "com.apple.eawt.QuitHandler";
  static final String APP_EVENT_LISTENER_CLASS_NAME = "com.apple.eawt.AppEventListener";
  static final String APP_REOPENED_LISTENER_CLASS_NAME = "com.apple.eawt.AppReOpenedListener";

  static final String QUIT_RESPONSE_CLASS_NAME = "com.apple.eawt.QuitResponse";
  static final String OPEN_FILES_EVENT_CLASS_NAME = "com.apple.eawt.AppEvent$OpenFilesEvent";

  // METHODS

  public static void installDefaultMenuBar( final JMenuBar defaultMenuBar )
  {
    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    try
    {
      Class<?> appClass = classLoader.loadClass( APPLICATION_CLASS_NAME );
      if ( appClass != null )
      {
        // Call Application#getApplication() ...
        Method getAppMethod = appClass.getMethod( "getApplication" );
        Object app = getAppMethod.invoke( null );

        // Call Application#setDefaultMenuBar() ...
        Method setDefaultMenuBarMethod = appClass.getMethod( "setDefaultMenuBar", JMenuBar.class );
        setDefaultMenuBarMethod.invoke( app, defaultMenuBar );
      }
    }
    catch ( Exception exception )
    {
      System.err.printf( "[WARNING] Installation of default menubar failed: %s", exception.getMessage() );
    }
  }

  public static void installPlatformCallback( final PlatformCallback callback )
  {
    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    try
    {
      Class<?> aboutHandlerClass = classLoader.loadClass( ABOUT_HANDLER_CLASS_NAME );
      Object aboutHandler = OSXAboutHandler.createInstance( classLoader, callback, aboutHandlerClass );

      Class<?> openFilesHandlerClass = classLoader.loadClass( OPEN_FILES_HANDLER_CLASS_NAME );
      Object openFilesHandler = OSXOpenFilesHandler.createInstance( classLoader, callback, openFilesHandlerClass );

      Class<?> prefsHandlerClass = classLoader.loadClass( PREFS_HANDLER_CLASS_NAME );
      Object prefsHandler = OSXPrefsHandler.createInstance( classLoader, callback, prefsHandlerClass );

      Class<?> quitHandlerClass = classLoader.loadClass( QUIT_HANDLER_CLASS_NAME );
      Object quitHandler = OSXQuitHandler.createInstance( classLoader, callback, quitHandlerClass );

      Class<?> appEventListenerClass = classLoader.loadClass( APP_EVENT_LISTENER_CLASS_NAME );
      Class<?> appReOpenedListenerClass = classLoader.loadClass( APP_REOPENED_LISTENER_CLASS_NAME );
      Object appReOpenedListenerHandler = OSXAppReOpenedListenerHandler.createInstance( classLoader, callback,
          appEventListenerClass, appReOpenedListenerClass );

      Class<?> appClass = classLoader.loadClass( APPLICATION_CLASS_NAME );
      if ( appClass != null )
      {
        // Call Application#getApplication() ...
        Method getAppMethod = appClass.getMethod( "getApplication" );
        Object app = getAppMethod.invoke( null );

        // Call Application#addAppEventListener() ...
        Method addAppEventListenerMethod = appClass.getMethod( "addAppEventListener", appEventListenerClass );
        addAppEventListenerMethod.invoke( app, appReOpenedListenerHandler );

        // Call Application#setAboutHandler() ...
        Method setAboutHandlerMethod = appClass.getMethod( "setAboutHandler", aboutHandlerClass );
        setAboutHandlerMethod.invoke( app, aboutHandler );

        // Call Application#setOpenFilesHandler() ...
        Method setOpenFilesHandlerMethod = appClass.getMethod( "setOpenFileHandler", openFilesHandlerClass );
        setOpenFilesHandlerMethod.invoke( app, openFilesHandler );

        // Call Application#setPreferencesHandler() ...
        Method setPreferencesHandlerMethod = appClass.getMethod( "setPreferencesHandler", prefsHandlerClass );
        setPreferencesHandlerMethod.invoke( app, prefsHandler );

        // Call Application#setQuitHandler() ...
        Method setQuitHandlerMethod = appClass.getMethod( "setQuitHandler", quitHandlerClass );
        setQuitHandlerMethod.invoke( app, quitHandler );
      }
    }
    catch ( Exception exception )
    {
      System.err.printf( "[WARNING] Installation of platform callback failed: %s", exception.getMessage() );
    }
  }

  public static void uninstallPlatformCallback()
  {
    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    try
    {
      Class<?> aboutHandlerClass = classLoader.loadClass( ABOUT_HANDLER_CLASS_NAME );
      Class<?> openFilesHandlerClass = classLoader.loadClass( OPEN_FILES_HANDLER_CLASS_NAME );
      Class<?> prefsHandlerClass = classLoader.loadClass( PREFS_HANDLER_CLASS_NAME );
      Class<?> quitHandlerClass = classLoader.loadClass( QUIT_HANDLER_CLASS_NAME );
      Class<?> appEventListenerClass = classLoader.loadClass( APP_EVENT_LISTENER_CLASS_NAME );

      Class<?> appClass = classLoader.loadClass( APPLICATION_CLASS_NAME );
      if ( appClass != null )
      {
        // Call Application#getApplication() ...
        Method getAppMethod = appClass.getMethod( "getApplication" );
        Object app = getAppMethod.invoke( null );

        // Call Application#addAppEventListener() ...
        Method addAppEventListenerMethod = appClass.getMethod( "addAppEventListener", appEventListenerClass );
        addAppEventListenerMethod.invoke( app, new Object[] { null } );

        // Call Application#setAboutHandler() ...
        Method setAboutHandlerMethod = appClass.getMethod( "setAboutHandler", aboutHandlerClass );
        setAboutHandlerMethod.invoke( app, new Object[] { null } );

        // Call Application#setOpenFilesHandler() ...
        Method setOpenFilesHandlerMethod = appClass.getMethod( "setOpenFileHandler", openFilesHandlerClass );
        setOpenFilesHandlerMethod.invoke( app, new Object[] { null } );

        // Call Application#setPreferencesHandler() ...
        Method setPreferencesHandlerMethod = appClass.getMethod( "setPreferencesHandler", prefsHandlerClass );
        setPreferencesHandlerMethod.invoke( app, new Object[] { null } );

        // Call Application#setQuitHandler() ...
        Method setQuitHandlerMethod = appClass.getMethod( "setQuitHandler", quitHandlerClass );
        setQuitHandlerMethod.invoke( app, new Object[] { null } );
      }
    }
    catch ( Exception exception )
    {
      System.err.printf( "[WARNING] Uninstallation of platform callback failed: %s", exception.getMessage() );
    }
  }
}
