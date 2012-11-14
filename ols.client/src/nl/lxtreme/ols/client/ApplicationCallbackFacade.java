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


import java.lang.reflect.*;

import org.osgi.service.log.*;


/**
 * Service facade for {@link ApplicationCallback}.
 */
public final class ApplicationCallbackFacade implements ApplicationCallback
{
  // INNER TYPES

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

  private volatile ApplicationCallback service;
  private volatile LogService log;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handleAbout()
  {
    return this.service.handleAbout();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handlePreferences()
  {
    return this.service.handlePreferences();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean handleQuit()
  {
    return this.service.handleQuit();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasPreferences()
  {
    return this.service.hasPreferences();
  }

  /**
   * Called by our dependency manager in case both dependencies are available.
   */
  public void start()
  {
    installApplicationCallback( this );
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
      this.log.log( LogService.LOG_WARNING, "Install application callback failed!", exception );
    }
  }
}
