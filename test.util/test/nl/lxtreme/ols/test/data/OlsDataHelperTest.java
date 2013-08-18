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
package nl.lxtreme.ols.test.data;


import static org.junit.Assert.*;

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.test.data.project.*;
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

  private StubDataSet dataSet;
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
    this.dataSet = new StubDataSet();

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
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertCursorSet( 0, 1234 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorBOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorB: 1234\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertCursorSet( 1, 1234 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorSetOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Cursor2: 1234\n;Cursor8: 2345\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertCursorUnset( 0 );
    this.dataSet.assertCursorUnset( 1 );
    this.dataSet.assertCursorSet( 2, 1234 );
    this.dataSet.assertCursorUnset( 3 );
    this.dataSet.assertCursorUnset( 4 );
    this.dataSet.assertCursorUnset( 5 );
    this.dataSet.assertCursorUnset( 6 );
    this.dataSet.assertCursorUnset( 7 );
    this.dataSet.assertCursorSet( 8, 2345 );
    this.dataSet.assertCursorUnset( 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileMissingAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 1, 3 );
    this.dataSet.assertValues( 0, 1, 3 );
    this.dataSet.assertAbsoluteLength( 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingChannelsFail() throws Exception
  {
    final String snippet = ";Rate: 1\n0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0 );
    this.dataSet.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingSampleDataFail() throws Exception
  {
    final String snippet = MINIMAL_HEADER;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0 );
    this.dataSet.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingSampleRateFail() throws Exception
  {
    final String snippet = ";Channels: 1\n0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0 );
    this.dataSet.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileSizeMismatchFail() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 2\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0 );
    this.dataSet.assertValues( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileWithAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n;AbsoluteLength: 9\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 1, 3, 9 );
    this.dataSet.assertValues( 0, 1, 3, 3 );
    this.dataSet.assertAbsoluteLength( 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileWithInvalidAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n;AbsoluteLength: 2\n0@0\n1@1\n3@3";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 1, 3 );
    this.dataSet.assertValues( 0, 1, 3 );
    this.dataSet.assertAbsoluteLength( 3 );
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
        OlsDataHelper.read( this.dataSet, reader );

        assertNotNull( this.dataSet.getCapturedData() );
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
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( Integer.MIN_VALUE, Integer.MIN_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadInvalidTimeValueOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@" + Long.MIN_VALUE;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0 );
    this.dataSet.assertValues( 0 );
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
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( Integer.MAX_VALUE, Integer.MAX_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxSampleValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + Integer.toHexString( Integer.MAX_VALUE ) + "@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( Integer.MAX_VALUE, Integer.MAX_VALUE );
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
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( Long.MAX_VALUE, Long.MAX_VALUE );
    this.dataSet.assertValues( 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxTimeValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@" + Long.MAX_VALUE;

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( Long.MAX_VALUE, Long.MAX_VALUE );
    this.dataSet.assertValues( 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinimalDataFileOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( 0, 0 );
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
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinSampleValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( 0, 0 );
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
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinTimeValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 0 );
    this.dataSet.assertValues( 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMSBDataOk() throws Exception
  {
    // Issue #80: MSB data isn't correctly read back from stored data...
    final String snippet = MINIMAL_HEADER + "80000000@0\n00000000@1\n80000000@2\n";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertTimeStamps( 0, 1, 2 );
    this.dataSet.assertValues( 0x80000000, 0x0, 0x80000000 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadNegativeEnabledChannelsOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";EnabledChannels: " + 0xFF000000 + "\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertChannelGroupDisabled( 0 );
    this.dataSet.assertChannelGroupDisabled( 1 );
    this.dataSet.assertChannelGroupDisabled( 2 );
    this.dataSet.assertChannelGroupEnabled( 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadOldUnsetCursorOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorA: " + Long.MIN_VALUE + "\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertCursorUnset( 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadPositiveEnabledChannelsOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";EnabledChannels: " + 0x00FF0000 + "\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertChannelGroupDisabled( 0 );
    this.dataSet.assertChannelGroupDisabled( 1 );
    this.dataSet.assertChannelGroupEnabled( 2 );
    this.dataSet.assertChannelGroupDisabled( 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadUnsetCursorOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Cursor9: " + Long.MIN_VALUE + "\n" + "0@0";

    final StringReader reader = new StringReader( snippet );
    OlsDataHelper.read( this.dataSet, reader );

    this.dataSet.assertCursorUnset( 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testSimpleWriteOk() throws Exception
  {
    this.dataSet.setCapturedData( new CapturedData( new int[] { 1 }, new long[] { 2L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.dataSet, writer );

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
    this.dataSet.setCapturedData( new CapturedData( new int[] { -1 }, new long[] { 1L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.dataSet, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( "ffffffff@1" ) );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testWriteInvalidTimestampOk() throws Exception
  {
    this.dataSet.setCapturedData( new CapturedData( new int[] { 1 }, new long[] { -1L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.dataSet, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( "1@" + Long.MAX_VALUE ) );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testWriteMSBDataOk() throws Exception
  {
    // Issue #80: MSB data isn't correctly written to stored data...
    this.dataSet.setCapturedData( new CapturedData( new int[] { 0x80000000, 0x0, 0x80000000 },
        new long[] { 0L, 1L, 2L }, -1, 100, 2, 2, 1 ) );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( this.dataSet, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( "80000000@0" ) );
    assertTrue( snippet.contains( "00000000@1" ) );
    assertTrue( snippet.contains( "80000000@2" ) );
  }
}
