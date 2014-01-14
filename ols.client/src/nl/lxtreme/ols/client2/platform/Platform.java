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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.platform;


import java.awt.*;

import nl.lxtreme.ols.client2.platform.osx.*;


/**
 * Platform specific code.
 */
public final class Platform
{
  // METHODS

  /**
   * Makes the default system sound.
   */
  public static void beep()
  {
    Toolkit.getDefaultToolkit().beep();
  }

  /**
   * Initializes the platform.
   */
  public static void initPlatform()
  {
    if ( isMacOS() )
    {
      // Moves the main menu bar to the screen menu bar location...
      System.setProperty( "apple.laf.useScreenMenuBar", "true" );
      System.setProperty( "apple.awt.textantialiasing", "true" );
      System.setProperty( "apple.awt.graphics.EnableQ2DX", "true" );
      System.setProperty( "com.apple.mrj.application.growbox.intrudes", "false" );
      System.setProperty( "com.apple.mrj.application.live-resize", "false" );
      System.setProperty( "com.apple.macos.smallTabs", "true" );
      System.setProperty( "apple.eawt.quitStrategy", "CLOSE_ALL_WINDOWS" );
    }
  }

  /**
   * Installs a given {@link ApplicationCallback} for the current platform.
   * 
   * @param aCallback
   *          the {@link ApplicationCallback} to install, cannot be
   *          <code>null</code>.
   */
  public static void installApplicationCallback( ApplicationCallback aCallback )
  {
    if ( isMacOS() )
    {
      OSXApplicationCallback.installApplicationCallback( aCallback );
    }
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
   * Returns whether the current host's operating system is Windows.
   * 
   * @return <code>true</code> if running on Windows, <code>false</code>
   *         otherwise.
   */
  public static boolean isWindows()
  {
    final String osName = System.getProperty( "os.name" );
    return osName.indexOf( "win" ) >= 0;
  }
}
