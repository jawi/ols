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
package nl.lxtreme.ols.client.ui.action;


import static nl.lxtreme.ols.client.ui.Platform.*;
import static nl.lxtreme.ols.client.ui.action.FileExtensionUtils.*;

import java.io.*;

import junit.framework.*;


/**
 * 
 */
public class FileExtensionUtilsTest extends TestCase
{
  // METHODS

  /**
   * Tests that getting a file extension under various circumstances works
   * correctly on Unix-derivatives.
   * 
   * @see HostUtils#getFileExtension(java.io.File)
   */
  public void testGetFileExtensionOnUnixDerivatives()
  {
    if ( !isUnix() && !isMacOS() )
    {
      return;
    }

    File f;

    f = new File( "/tmp/test" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( "txt", getFileExtension( f ) );

    f = new File( "/does/not/exist/test.txt" );
    assertEquals( "txt", getFileExtension( f ) );

    f = new File( "/does/not/exist/test" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( "", "" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( ".", ".txt" );
    assertEquals( "txt", getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-dir" );
    assertTrue( f.mkdirs() );
    f.deleteOnExit();

    assertEquals( "", getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-dir.tmp" );
    assertTrue( f.mkdirs() );
    f.deleteOnExit();

    assertEquals( "", getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-file2.tmp" );

    assertEquals( "tmp", getFileExtension( f ) );
  }

  /**
   * Tests that getting a file extension under various circumstances works
   * correctly on the Windows platform.
   * 
   * @see HostUtils#getFileExtension(java.io.File)
   */
  public void testGetFileExtensionOnWindows()
  {
    if ( !isWindows() )
    {
      return;
    }

    File f;

    f = new File( System.getProperty( "user.home" ) );
    assertEquals( "", getFileExtension( f ) );

    f = new File( "c:\\does-not-exist\\test" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( "txt", getFileExtension( f ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( "txt", getFileExtension( f ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( "", "" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( ".", ".txt" );
    assertEquals( "txt", getFileExtension( f ) );

    f = new File( "c:\\" );
    assertEquals( "", getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-dir" );
    f.mkdirs();
    f.deleteOnExit();

    assertEquals( "", getFileExtension( f ) );
  }

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
    assertEquals( new File( f, ".test" ), setFileExtension( f, "test" ) );

    f = new File( "/tmp/test" );
    assertEquals( new File( "/tmp/test.txt" ), setFileExtension( f, "txt" ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( f, setFileExtension( f, "txt" ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( f, setFileExtension( f, ".txt" ) );

    f = new File( "/does/not/exist/test.txt" );
    assertEquals( f, setFileExtension( f, "txt" ) );

    f = new File( "/does/not/exist/test" );
    assertEquals( new File( "/does/not/exist/test.txt" ), setFileExtension( f, "txt" ) );

    f = new File( "", "" );
    assertEquals( new File( "", ".txt" ), setFileExtension( f, "txt" ) );

    f = new File( ".", "" );
    assertEquals( new File( ".", ".txt" ), setFileExtension( f, "txt" ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-file" );
    f.deleteOnExit();

    createTestFile( f );

    assertEquals( new File( System.getProperty( "java.io.tmpdir" ), "test-file.txt" ), setFileExtension( f, "txt" ) );
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
    assertEquals( new File( f, ".test" ), setFileExtension( f, "test" ) );

    f = new File( "c:\\does-not-exist\\test" );
    assertEquals( new File( "c:\\does-not-exist\\test.txt" ), setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( f, setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\testtxt" );
    assertEquals( f, setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( f, setFileExtension( f, ".txt" ) );

    f = new File( "c:\\does-not-exist\\testtxt" );
    assertEquals( new File( "c:\\does-not-exist\\testtxt" ), setFileExtension( f, ".txt" ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( f, setFileExtension( f, "txt" ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( new File( "q:\\does-not-exist\\test.txt" ), setFileExtension( f, "txt" ) );

    f = new File( "", "" );
    assertEquals( new File( "", ".txt" ), setFileExtension( f, "txt" ) );

    f = new File( ".", "" );
    assertEquals( new File( ".", ".txt" ), setFileExtension( f, "txt" ) );
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
}
