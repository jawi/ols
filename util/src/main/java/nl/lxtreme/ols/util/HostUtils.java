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


import java.io.*;
import java.util.logging.*;

import org.osgi.framework.Version;


/**
 * Provides some host/OS specific utilities.
 */
public final class HostUtils implements HostInfo
{
  // INNER TYPES

  // CONSTANTS

  private static final HostInfo HOSTINFO = new HostUtils();

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
    if ( HOSTINFO.isMacOS() )
    {
      // This is the location where to store data on MacOS...
      dirName = System.getProperty( "user.home" ) + "/Library/Preferences";
      fileName = aName + ".Application";
    }
    else if ( HOSTINFO.isUnix() )
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
      while ( aResource.read() >= 0 )
      {
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

    // Avoid directories being detected as having a file extension...
    if ( aFile.exists() && aFile.isDirectory() && !"".equals( ext ) )
    {
      ext = "";
    }

    return ext;
  }

  /**
   * Returns the current value of hostinfo.
   *
   * @return the host information, never <code>null</code>.
   */
  public static HostInfo getHostInfo()
  {
    return HOSTINFO;
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

  @Override
  public int getJavaVersion()
  {
    String prop = System.getProperty( "java.version" );
    if ( prop.contains( "_" ) )
    {
      prop = prop.replaceFirst( "_", "." );
    }
    Version version = Version.parseVersion( prop );
    return version.getMajor();
  }

  /**
   * Returns whether the current host's operating system is Linux or any other
   * UNIX-like operating system, such as Solaris (SunOS).
   *
   * @return <code>true</code> if running on Linux or any other UNIX system,
   *         <code>false</code> otherwise.
   */
  public boolean isLinux()
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
  public boolean isMacOS()
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
  public boolean isSolaris()
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
  public boolean isUnix()
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
  public boolean isWindows()
  {
    final String osName = System.getProperty( "os.name" ).toLowerCase();
    return osName.indexOf( "win" ) >= 0;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean needsAboutMenuItem()
  {
    return !isMacOS();
  }

  /**
   * {@inheritDoc}
   */
  public final boolean needsExitMenuItem()
  {
    return !isMacOS();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean needsPreferencesMenuItem()
  {
    return !isMacOS();
  }
}

/* EOF */
