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
package nl.lxtreme.ols.util;


import static org.junit.Assert.*;

import java.io.*;

import org.junit.*;


/**
 *
 */
public class HostUtilsTest
{
  // TESTS

  /**
   * Tests that getting a file extension under various circumstances works
   * correctly.
   * 
   * @see HostUtils#getFileExtension(java.io.File)
   */
  @Test
  @edu.umd.cs.findbugs.annotations.SuppressWarnings( value = "DMI_HARDCODED_ABSOLUTE_FILENAME", justification = "this is a test!" )
  public void testGetFileExtension()
  {
    File f = new File( System.getProperty( "user.home" ) );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "c:\\temp\\test" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

    f = new File( "c:\\temp\\test.txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( "txt", HostUtils.getFileExtension( f ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( "", HostUtils.getFileExtension( f ) );

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

    f = new File( "c:\\" );
    assertEquals( "", HostUtils.getFileExtension( f ) );
  }

  /**
   * Tests that setting a file extension under various circumstances works
   * correctly.
   * 
   * @see HostUtils#setFileExtension(java.io.File, String)
   */
  @Test
  @edu.umd.cs.findbugs.annotations.SuppressWarnings( value = "DMI_HARDCODED_ABSOLUTE_FILENAME", justification = "this is a test!" )
  public void testSetFileExtension()
  {
    File f = new File( System.getProperty( "user.home" ) );
    assertEquals( new File( f, ".test" ), HostUtils.setFileExtension( f, "test" ) );

    f = new File( "c:\\temp\\test" );
    assertEquals( new File( "c:\\temp\\test.txt" ), HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\temp\\test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "c:\\temp\\test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, ".txt" ) );

    f = new File( "q:\\does-not-exist\\test.txt" );
    assertEquals( f, HostUtils.setFileExtension( f, "txt" ) );

    f = new File( "q:\\does-not-exist\\test" );
    assertEquals( new File( "q:\\does-not-exist\\test.txt" ), HostUtils.setFileExtension( f, "txt" ) );

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
  }
}
