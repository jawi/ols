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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.io.serial.test;


/**
 * 
 */
class HostUtils
{

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
