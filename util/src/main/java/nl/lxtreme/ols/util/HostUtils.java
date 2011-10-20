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
package nl.lxtreme.ols.util;


import java.awt.event.*;
import java.io.*;
import java.lang.reflect.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;


/**
 * Provides some host/OS specific utilities.
 */
public final class HostUtils
{
  // INNER TYPES

  /**
   * Denotes a callback for some "main events" in an application.
   * <p>
   * This interface is merely used for retrieving the proper system events on
   * Mac OSX. On other operating systems, this interface has no real value.
   * </p>
   */
  public interface ApplicationCallback
  {
    /**
     * Called upon receiving a "about" event from the host operating system.
     * 
     * @return <code>true</code> if the event is being handled,
     *         <code>false</code> (the default) if this event is ignored.
     */
    public boolean handleAbout();

    /**
     * Called upon receiving a "set preferenes" event from the host operating
     * system.
     * 
     * @return <code>true</code> if the event is being handled,
     *         <code>false</code> (the default) if this event is ignored.
     */
    public boolean handlePreferences();

    /**
     * Called upon receiving a quit event from the host operating system.
     * 
     * @return <code>true</code> if the event is being handled,
     *         <code>false</code> (the default) if this event is ignored.
     */
    public boolean handleQuit();

    /**
     * Returns whether the are preferences to configure.
     * 
     * @return <code>true</code> if there are preferences to configure,
     *         <code>false</code> otherwise.
     */
    public boolean hasPreferences();
  }

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
  static final class CLValue implements UIDefaults.ActiveValue
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

        final Object quitResponseObj = aArgs[1];
        final Class<?> quitResponseClass = quitResponseObj.getClass();
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
      return null;
    }
  }

  // CONSTRUCTORS

  /**
   * Creates a new HostUtils instance.
   */
  private HostUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Closes a given resource.
   * <p>
   * If the given resource also implements the {@link Flushable} interface, the
   * resource is flushed before being closed.
   * </p>
   * 
   * @param aResource
   *          the resource to close, can be <code>null</code>, it might already
   *          be closed.
   * @return <code>true</code> if the close operation succeeded,
   *         <code>false</code> if it is unsure whether it succeeded.
   */
  public static final boolean closeResource( final Closeable aResource )
  {
    boolean result = false;
    if ( aResource != null )
    {
      try
      {
        if ( aResource instanceof Flushable )
        {
          ( ( Flushable )aResource ).flush();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }
      finally
      {
        try
        {
          aResource.close();
          // Indicate success...
          result = true;
        }
        catch ( IOException exception )
        {
          // Ignore...
        }
      }

    }
    return result;
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
  public static final File createLocalDataFile( final String aName, final String aExtension )
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
   * Creates an OS-specific file location to store properties.
   * 
   * @param aName
   *          the name of the properties file, excluding <tt>.properties</tt>,
   *          cannot be <code>null</code> or empty. By convention, the name of a
   *          properties file should be in the "reverse package name", e.g.,
   *          "com.foo.bar".
   * @return the file pointing to the OS-specific properties file location,
   *         never <code>null</code>.
   */
  public static final File createLocalPropertiesFile( final String aName )
  {
    return createLocalDataFile( aName, "properties" );
  }

  /**
   * Flushes the given input stream by reading as many bytes as there are still
   * available.
   * 
   * @param aResource
   *          the resource to flush, can be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems/
   */
  public static final void flushInputStream( final InputStream aResource ) throws IOException
  {
    if ( aResource != null )
    {
      while ( aResource.available() > 0 )
      {
        aResource.read();
      }
    }
  }

  /**
   * Returns the "presumed" filename extension (like '.jpg', '.zip') from a
   * given file.
   * 
   * @param aFile
   *          the file to return the extension for, cannot be <code>null</code>.
   * @return the file extension (always in lower case), never <code>null</code>
   *         but can be empty if the given file has <em>no</em> file extension.
   */
  public static final String getFileExtension( final File aFile )
  {
    String ext = "";

    String filename = aFile.getName();
    int idx = filename.lastIndexOf( '.' );

    if ( ( idx >= 0 ) && ( idx < ( filename.length() - 1 ) ) )
    {
      ext = filename.substring( idx + 1 ).toLowerCase();
    }
    return ext;
  }

  /**
   * This method calls Thread.currentThread().interrupt() if any exception in
   * the hierarchy (including all parent causes) is either an
   * {@link InterruptedIOException} or {@link InterruptedException}. This method
   * should be called in every catch(IOException), catch(Exception) or
   * catch(Throwable) block.
   * 
   * @param aThrowable
   *          the exception to be checked for interruption. Does nothing if
   *          <code>null</code>.
   * @return <code>true</code> if the exception is "handled" by this method,
   *         <code>false</code> otherwise.
   */
  public static boolean handleInterruptedException( final Throwable aThrowable )
  {
    if ( aThrowable == null )
    {
      return true;
    }

    Throwable current = aThrowable;
    do
    {
      if ( ( current instanceof InterruptedIOException ) || ( current instanceof InterruptedException ) )
      {
        Thread.currentThread().interrupt();
        return true;
      }
      current = current.getCause();
    }
    while ( current != null );

    return false;
  }

  /**
   * Allows the logging properties of the JVM to be set at any moment in time
   * providing the logging configuration in an input-stream.
   * 
   * @param aInputStream
   *          the input stream providing the logging properties, cannot be
   *          <code>null</code>.
   */
  public static final void initLogging( final InputStream aInputStream )
  {
    final LogManager logManager = LogManager.getLogManager();
    final ClassLoader cl = Thread.currentThread().getContextClassLoader();
    try
    {
      Thread.currentThread().setContextClassLoader( HostUtils.class.getClassLoader() );
      logManager.readConfiguration( aInputStream );
    }
    catch ( IOException exception )
    {
      Logger.getAnonymousLogger().log( Level.FINE, "Problems to load the logging configuration file!", exception );
    }
    finally
    {
      Thread.currentThread().setContextClassLoader( cl );
    }
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
  public static final void initOSSpecifics( final String aApplicationName,
      final ApplicationCallback aApplicationCallback )
  {
    if ( isMacOS() )
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
    else if ( isUnix() )
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
    else if ( isWindows() )
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
   * Returns whether the current host's operating system is Linux or any other
   * UNIX-like operating system, such as Solaris (SunOS).
   * 
   * @return <code>true</code> if running on Linux or any other UNIX system,
   *         <code>false</code> otherwise.
   */
  public static boolean isLinux()
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
  public static final boolean isMacOS()
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
  public static boolean isSolaris()
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
  public static boolean isUnix()
  {
    String osName = System.getProperty( "os.name" ).toLowerCase();
    return ( osName.indexOf( "nix" ) >= 0 ) || //
        // linux
        isLinux() ||
        // solaris
        isSolaris();
  }

  /**
   * Returns whether the current host's operating system is Windows.
   * 
   * @return <code>true</code> if running on Windows, <code>false</code>
   *         otherwise.
   */
  public static boolean isWindows()
  {
    final String osName = System.getProperty( "os.name" ).toLowerCase();
    return osName.indexOf( "win" ) >= 0;
  }

  /**
   * Returns whether the host OS needs an explicit exit menu item or not.
   * <p>
   * For example, on Mac OS, you don't need an explicit exit menu, since it is
   * by default provided. On Linux or Windows machines, you do need an explicit
   * exit function.
   * </p>
   * 
   * @return <code>true</code> if this host needs an explicit exit menu item,
   *         <code>false</code> otherwise.
   */
  public static final boolean needsExitMenuItem()
  {
    return !isMacOS();
  }

  /**
   * Reverses the elements in the given array.
   * 
   * @param aArray
   *          the array to reverse, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given array was <code>null</code>.
   */
  public static final void reverse( final int[] aArray )
  {
    if ( aArray == null )
    {
      throw new IllegalArgumentException( "Array cannot be null!" );
    }

    for ( int left = 0, right = aArray.length - 1; left < right; left++, right-- )
    {
      // exchange the first and last
      int temp = aArray[left];
      aArray[left] = aArray[right];
      aArray[right] = temp;
    }
  }

  /**
   * Reverses the elements in the given array.
   * 
   * @param aArray
   *          the array to reverse, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given array was <code>null</code>.
   */
  public static final <T> void reverse( final T[] aArray )
  {
    if ( aArray == null )
    {
      throw new IllegalArgumentException( "Array cannot be null!" );
    }

    for ( int left = 0, right = aArray.length - 1; left < right; left++, right-- )
    {
      // exchange the first and last
      T temp = aArray[left];
      aArray[left] = aArray[right];
      aArray[right] = temp;
    }
  }

  /**
   * Sets the filename to end with the given file extension, if this is not
   * already the case.
   * 
   * @param aFile
   *          the file that should get the given file extension, cannot be
   *          <code>null</code>;
   * @param aFileExtension
   *          the new file extension to add to the given file, cannot be
   *          <code>null</code>.
   * @return a file with the given file extension, never <code>null</code>.
   */
  public static final File setFileExtension( final File aFile, final String aFileExtension )
  {
    // Take care of any given periods in the extension...
    String extension = aFileExtension.trim();
    if ( extension.startsWith( "." ) )
    {
      extension = extension.substring( 1 );
    }

    // If the filename already has the given file extension, than simply do
    // nothing...
    if ( extension.isEmpty() )
    {
      return aFile;
    }

    File directory = aFile;
    String filename = "";
    boolean endsWithExtension = aFile.getName().toLowerCase().endsWith( extension.toLowerCase() );

    if ( !aFile.isDirectory() || endsWithExtension )
    {
      filename = aFile.getName();
      directory = aFile.getParentFile();
    }

    if ( !endsWithExtension )
    {
      filename = filename.concat( "." ).concat( aFileExtension );
    }

    return new File( directory, filename );
  }

  /**
   * Returns the "presumed" filename extension (like '.jpg', '.zip') from a
   * given file.
   * 
   * @param aFile
   *          the file to return the extension for, cannot be <code>null</code>.
   * @return the file extension (always in lower case), never <code>null</code>
   *         but can be empty if the given file has <em>no</em> file extension.
   */
  public static final String stripFileExtension( final File aFile, final String... aExtensions )
  {
    return stripFileExtension( aFile.getName(), aExtensions );
  }

  /**
   * Returns the "presumed" filename extension (like '.jpg', '.zip') from a
   * given file.
   * 
   * @param aFilename
   *          the name of the file to strip the extension from, cannot be
   *          <code>null</code>;
   * @param aExtensions
   *          the (optional) extensions that should be stripped. If omitted,
   *          everything behind the last dot will be removed.
   * @return the file extension (always in lower case), never <code>null</code>
   *         but can be empty if the given file has <em>no</em> file extension.
   */
  public static final String stripFileExtension( final String aFilename, final String... aExtensions )
  {
    String result = "";

    int idx = aFilename.lastIndexOf( '.' );
    if ( ( idx >= 0 ) && ( idx < ( aFilename.length() - 1 ) ) )
    {
      result = aFilename.substring( 0, idx );

      boolean found = ( aExtensions == null ) || ( aExtensions.length == 0 );

      final String ext = aFilename.substring( idx + 1 ).toLowerCase();
      if ( ( aExtensions != null ) && ( aExtensions.length > 0 ) )
      {
        for ( String extension : aExtensions )
        {
          if ( ext.equalsIgnoreCase( extension ) )
          {
            found = true;
            break;
          }
        }
      }

      if ( !found )
      {
        result = aFilename;
      }
    }

    return result;
  }

  /**
   * @param aApplicationCallback
   */
  private static void installApplicationCallback( final ApplicationCallback aApplicationCallback )
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
  private static final void setLookAndFeel( final String aLookAndFeelClassName )
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

/* EOF */
