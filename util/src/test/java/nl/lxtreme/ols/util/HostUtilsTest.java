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


import static org.junit.Assert.*;
import static org.junit.Assume.*;

import java.io.*;

import org.junit.*;


/**
 * Provides test cases for {@link HostUtils}.
 */
public class HostUtilsTest
{
  // VARIABLES

  private HostInfo hostInfo;

  // TESTS

  /**
   * Set up for each test case.
   */
  @Before
  public void setUp()
  {
    this.hostInfo = HostUtils.getHostInfo();
  }

  /**
   * Tests that getting a file extension under various circumstances works
   * correctly on Unix-derivatives.
   * 
   * @see HostUtils#getFileExtension(java.io.File)
   */
  @Test
  public void testGetFileExtensionOnUnixDerivatives()
  {
    assumeTrue( this.hostInfo.isUnix() || this.hostInfo.isMacOS() );

    File f;

    f = new File( "/tmp/test" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "/does/not/exist/test.txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "/does/not/exist/test" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "", "" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( ".", ".txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-dir" );
    assertTrue( f.mkdirs() );
    f.deleteOnExit();

    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-dir.tmp" );
    assertTrue( f.mkdirs() );
    f.deleteOnExit();

    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-file2.tmp" );

    assertEquals( "tmp", HostUtils.getFileExtension( f ) );
  }

  /**
   * Tests that getting a file extension under various circumstances works
   * correctly on the Windows platform.
   * 
   * @see HostUtils#getFileExtension(java.io.File)
   */
  @Test
  public void testGetFileExtensionOnWindows()
  {
    assumeTrue( this.hostInfo.isWindows() );

    File f;

    f = new File( System.getProperty( "user.home" ) );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "c:\\does-not-exist\\test" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "", "" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( ".", ".txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "c:\\" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-dir" );
    f.mkdirs();
    f.deleteOnExit();

    assertEquals( "", HostUtils.getFileExtension( f ) );
  }

  /**
   * Tests that setting a file extension under various circumstances works
   * correctly on Unix-derivatives.
   * 
   * @see HostUtils#setFileExtension(java.io.File, String)
   */
  @Test
  public void testSetFileExtensionOnUnixDerivatives()
  {
    assumeTrue( this.hostInfo.isUnix() || this.hostInfo.isMacOS() );

    File f = new File( System.getProperty( "user.home" ) );
    assertEquals( new File( f, ".test" ), HostUtils.setFileExtension( f, "test" ) );

    f = new File( "/tmp/test" );
    assertEquals( new File( "/tmp/test.txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "/tmp/test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, ".txt" ) );

    f = new File( "/does/not/exist/test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "/does/not/exist/test" );
    assertEquals( new File( "/does/not/exist/test.txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "", "" );
    assertEquals( new File( "", ".txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( ".", "" );
    assertEquals( new File( ".", ".txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( System.getProperty( "java.io.tmpdir" ), "test-file" );
    f.deleteOnExit();

    createTestFile( f );

    assertEquals( new File( System.getProperty( "java.io.tmpdir" ), "test-file.txt" ),
        HostUtils.setFileExtension( f, "txt" ) );
  }

  /**
   * Tests that setting a file extension under various circumstances works
   * correctly on Windows platforms.
   * 
   * @see HostUtils#setFileExtension(java.io.File, String)
   */
  @Test
  public void testSetFileExtensionOnWindows()
  {
    assumeTrue( this.hostInfo.isWindows() );

    File f = new File( System.getProperty( "user.home" ) );
    assertEquals( new File( f, ".test" ), HostUtils.setFileExtension( f, "test" ) );

    f = new File( "c:\\does-not-exist\\test" );
    assertEquals( new File( "c:\\does-not-exist\\test.txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\testtxt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\does-not-exist\\test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, ".txt" ) );

    f = new File( "c:\\does-not-exist\\testtxt" );
    assertEquals( new File( "c:\\does-not-exist\\testtxt" ), HostUtils.setFileExtension( f, ".txt" ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( new File( "q:\\does-not-exist\\test.txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "", "" );
    assertEquals( new File( "", ".txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( ".", "" );
    assertEquals( new File( ".", ".txt" ), HostUtils.setFileExtension( f, "txt" ) );
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
