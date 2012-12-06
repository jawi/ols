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
package nl.lxtreme.ols.client.ui;


/**
 * Provides some common utility methods which provides information about the
 * running platform for use in the main client code.
 */
public class Platform
{
  // CONSTANTS

  private static final String SHORT_NAME = "OLS Client";
  private static final String FULL_NAME = SHORT_NAME.concat( " - Logic Analyzer Client" );

  // CONSTRUCTORS

  /**
   * Creates a new {@link Platform} instance, not used.
   */
  private Platform()
  {
    // Not used...
  }

  // METHODS

  /**
   * @return a string describing the execution environment, never
   *         <code>null</code>.
   */
  public static String getExecutionEnvironment()
  {
    return System.getProperty( "java.vendor" ) + ", v" + System.getProperty( "java.version" );
  }

  /**
   * @return the full name of the client, never <code>null</code>.
   */
  public static String getFullName()
  {
    return FULL_NAME;
  }

  /**
   * Returns the current value of shortName.
   * 
   * @return the shortName
   */
  public static String getShortName()
  {
    return SHORT_NAME;
  }

  /**
   * Returns whether or not debugging is enabled.
   * <p>
   * Useful for additional checks, logging and so on.
   * </p>
   * 
   * @return <code>true</code> if debug mode is enabled, <code>false</code>
   *         otherwise.
   */
  public static boolean isDebugMode()
  {
    return Boolean.getBoolean( "nl.lxtreme.ols.client.debug" );
  }

  /**
   * @return <code>true</code> if an explicit about menu item needs to be added
   *         to a UI created on the running host, <code>false</code> if the
   *         running host already provides an about menu item by default.
   */
  public static boolean needsAboutMenuItem()
  {
    return !isMacOS();
  }

  /**
   * @return <code>true</code> if an explicit exit menu item needs to be added
   *         to a UI created on the running host, <code>false</code> if the
   *         running host already provides an exit menu item by default.
   */
  public static boolean needsExitMenuItem()
  {
    return !isMacOS();
  }

  /**
   * @return <code>true</code> if an explicit preferences menu item needs to be
   *         added to a UI created on the running host, <code>false</code> if
   *         the running host already provides a preferences menu item by
   *         default.
   */
  public static boolean needsPreferencesMenuItem()
  {
    return !isMacOS();
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
  public static boolean isMacOS()
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
}
