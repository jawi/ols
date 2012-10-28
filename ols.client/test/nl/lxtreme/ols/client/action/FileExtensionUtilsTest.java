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
package nl.lxtreme.ols.client.action;


import java.io.*;

import junit.framework.*;


/**
 * 
 */
public class FileExtensionUtilsTest extends TestCase
{
  // METHODS

  /**
   * Tests that setting a file extension under various circumstances works
   * correctly on Unix-derivatives.
   * 
   * @see FileExtensionUtils#setFileExtension(java.io.File, String)
   */
  public void testSetFileExtensionOnUnixDerivatives()
  {
    if ( !isUnix() && !isMacOS() )
    {
      return;
    }

    File f = new File( System.getProperty( "user.home" ) );
    assertEquals( new File( f, ".test" ), FileExtensionUtils.setFileExtension( f, "test" ) );

    f = new File( "/tmp/test" );
    assertEquals( new File( "/tmp/test.txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, ".txt" ) );

    f = new File( "/does/not/exist/test.txt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "/does/not/exist/test" );
    assertEquals( new File( "/does/not/exist/test.txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "", "" );
    assertEquals( new File( "", ".txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( ".", "" );
    assertEquals( new File( ".", ".txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-file" );
    f.deleteOnExit();

    createTestFile( f );

    assertEquals( new File( System.getProperty( "java.io.tmpdir" ), "test-file.txt" ),
        FileExtensionUtils.setFileExtension( f, "txt" ) );
  }

  /**
   * Tests that setting a file extension under various circumstances works
   * correctly on Windows platforms.
   * 
   * @see FileExtensionUtils#setFileExtension(java.io.File, String)
   */
  public void testSetFileExtensionOnWindows()
  {
    if ( !isWindows() )
    {
      return;
    }

    File f = new File( System.getProperty( "user.home" ) );
    assertEquals( new File( f, ".test" ), FileExtensionUtils.setFileExtension( f, "test" ) );

    f = new File( "c:\\does-not-exist\\test" );
    assertEquals( new File( "c:\\does-not-exist\\test.txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\testtxt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, ".txt" ) );

    f = new File( "c:\\does-not-exist\\testtxt" );
    assertEquals( new File( "c:\\does-not-exist\\testtxt" ), FileExtensionUtils.setFileExtension( f, ".txt" ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( f, FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( new File( "q:\\does-not-exist\\test.txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( "", "" );
    assertEquals( new File( "", ".txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );

    f = new File( ".", "" );
    assertEquals( new File( ".", ".txt" ), FileExtensionUtils.setFileExtension( f, "txt" ) );
  }

  /**
   * @param f
   */
  private void createTestFile( final File f )
  {
    try
    {
      FileOutputStream fos = new FileOutputStream( f );
      fos.write( "test".getBytes() );
      fos.close();
    }
    catch ( Exception exception )
    {
      fail( exception.getMessage() );
    }
  }

  /**
   * Returns whether the current host's operating system is Linux or any other
   * UNIX-like operating system, such as Solaris (SunOS).
   * 
   * @return <code>true</code> if running on Linux or any other UNIX system,
   *         <code>false</code> otherwise.
   */
  private boolean isLinux()
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
  private boolean isMacOS()
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
  private boolean isSolaris()
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
  private boolean isUnix()
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
  private boolean isWindows()
  {
    final String osName = System.getProperty( "os.name" ).toLowerCase();
    return osName.indexOf( "win" ) >= 0;
  }

}
