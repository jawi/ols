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
package nl.lxtreme.ols.api.data;


import static org.junit.Assert.*;

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;


/**
 * Test cases for {@link OlsDataHelper}.
 */
public class OlsDataHelperTest
{
  // CONSTANTS

  private static final String MINIMAL_HEADER = ";Rate: 1\n;Channels: 8\n";

  // VARIABLES

  private ProjectImpl project;
  private List<File> dataFiles;

  // METHODS

  /**
   * Sets up the test case.
   * 
   * @throws IOException
   *           in case of exceptions.
   */
  @Before
  public void setUp() throws Exception
  {
    this.project = new ProjectImpl();

    this.dataFiles = new ArrayList<File>();

    final Enumeration<URL> resources = getClass().getClassLoader().getResources( "datafiles" );
    while ( resources.hasMoreElements() )
    {
      final URL resourceURL = resources.nextElement();
      final File resourceDir = new File( resourceURL.toURI() );
      this.dataFiles.addAll( Arrays.asList( resourceDir.listFiles() ) );
    }
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorAOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorA: 1234\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertCursorSet( 0, 1234 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorBOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorB: 1234\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertCursorSet( 1, 1234 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorSetOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Cursor2: 1234\n;Cursor8: 2345\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertCursorUnset( 0 );
    this.project.assertCursorUnset( 1 );
    this.project.assertCursorSet( 2, 1234 );
    this.project.assertCursorUnset( 3 );
    this.project.assertCursorUnset( 4 );
    this.project.assertCursorUnset( 5 );
    this.project.assertCursorUnset( 6 );
    this.project.assertCursorUnset( 7 );
    this.project.assertCursorSet( 8, 2345 );
    this.project.assertCursorUnset( 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileMissingAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0, 1, 3 );
    this.project.assertValues( 0, 1, 3 );
    this.project.assertAbsoluteLength( 3 + OlsDataHelper.ABS_TIME_MARGIN );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingChannelsFail() throws Exception
  {
    final String snippet = ";Rate: 1\n0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingSampleDataFail() throws Exception
  {
    final String snippet = MINIMAL_HEADER;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingSampleRateFail() throws Exception
  {
    final String snippet = ";Channels: 1\n0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileSizeMismatchFail() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 2\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileWithAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n;AbsoluteLength: 9\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0, 1, 3 );
    this.project.assertValues( 0, 1, 3 );
    this.project.assertAbsoluteLength( 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileWithInvalidAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n;AbsoluteLength: 2\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0, 1, 3 );
    this.project.assertValues( 0, 1, 3 );
    this.project.assertAbsoluteLength( 3 + OlsDataHelper.ABS_TIME_MARGIN );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadFileSetOk() throws Exception
  {
    for ( File dataFile : this.dataFiles )
    {
      System.err.println( "Reading " + dataFile );

      final FileReader reader = new FileReader( dataFile );
      try
      {
        OlsDataHelper.read( this.project, reader );

        assertNotNull( this.project.getCapturedData() );
      }
      finally
      {
        HostUtils.closeResource( reader );
      }
    }
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadInvalidSampleValueOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + Integer.toHexString( Integer.MIN_VALUE ) + "@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadInvalidTimeValueOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@" + Long.MIN_VALUE;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxSampleValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        Integer.toHexString( Integer.MAX_VALUE ) + "@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( Integer.MAX_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxSampleValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + Integer.toHexString( Integer.MAX_VALUE ) + "@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( Integer.MAX_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxTimeValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        "0@" + Long.MAX_VALUE;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( Long.MAX_VALUE );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxTimeValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@" + Long.MAX_VALUE;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( Long.MAX_VALUE );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinimalDataFileOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinSampleValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinSampleValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinTimeValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinTimeValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertTimeStamps( 0 );
    this.project.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadOldUnsetCursorOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorA: " + Long.MIN_VALUE + "\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertCursorUnset( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadUnsetCursorOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Cursor9: " + Long.MIN_VALUE + "\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.project, reader );

    this.project.assertCursorUnset( 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testSimpleWriteOk() throws Exception
  {
    this.project.setCapturedData( new CapturedDataImpl( new int[] { 1 }, new long[] { 2L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.project, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( ";Rate: 100" ) );
    assertTrue( snippet.contains( ";Channels: 2" ) );
    assertTrue( snippet.contains( ";EnabledChannels: 2" ) );
    assertTrue( snippet.contains( "1@2" ) );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testWriteInvalidSampleValueOk() throws Exception
  {
    this.project.setCapturedData( new CapturedDataImpl( new int[] { -1 }, new long[] { 1L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.project, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( Integer.toHexString( Integer.MAX_VALUE ) + "@1" ) );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testWriteInvalidTimestampOk() throws Exception
  {
    this.project.setCapturedData( new CapturedDataImpl( new int[] { 1 }, new long[] { -1L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.project, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( "1@" + Long.MAX_VALUE ) );
  }
}
