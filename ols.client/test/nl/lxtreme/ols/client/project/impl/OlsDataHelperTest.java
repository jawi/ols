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
package nl.lxtreme.ols.client.project.impl;


import static org.junit.Assert.*;

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.common.acquisition.*;

import org.junit.*;


/**
 * Test cases for {@link OlsDataHelper}.
 */
public class OlsDataHelperTest
{
  // CONSTANTS

  private static final String MINIMAL_HEADER = ";Rate: 1\n;Channels: 8\n";

  // VARIABLES

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
    this.dataFiles = new ArrayList<File>();

    String spec = getClass().getPackage().getName().replaceAll( "\\.", "/" );
    Enumeration<URL> resources = getClass().getClassLoader().getResources( spec );
    while ( resources.hasMoreElements() )
    {
      final File resourceDir = new File( resources.nextElement().toURI() );

      File[] dataFiles = resourceDir.listFiles( new FilenameFilter()
      {
        @Override
        public boolean accept( File aDir, String aName )
        {
          return aName.endsWith( ".ols" );
        }
      } );

      this.dataFiles.addAll( Arrays.asList( dataFiles ) );
    }
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorAOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorA: 1234\n" + "0@2000";

    assertCursorSet( readOlsData( snippet ), 0, 1234 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorBOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorB: 1234\n" + "0@2000";

    assertCursorSet( readOlsData( snippet ), 1, 1234 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadCursorSetOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Cursor2: 1234\n;Cursor8: 2345\n" + "0@3000";

    DataSet dataSet = readOlsData( snippet );

    assertCursorUnset( dataSet, 0 );
    assertCursorUnset( dataSet, 1 );
    assertCursorSet( dataSet, 2, 1234 );
    assertCursorUnset( dataSet, 3 );
    assertCursorUnset( dataSet, 4 );
    assertCursorUnset( dataSet, 5 );
    assertCursorUnset( dataSet, 6 );
    assertCursorUnset( dataSet, 7 );
    assertCursorSet( dataSet, 8, 2345 );
    assertCursorUnset( dataSet, 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileMissingAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n0@0\n1@1\n3@3";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 1, 3 );
    assertValues( dataSet, 0, 1, 3 );
    assertAbsoluteLength( dataSet, 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingChannelsFail() throws Exception
  {
    final String snippet = ";Rate: 1\n0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0 );
    assertValues( dataSet, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingSampleDataFail() throws Exception
  {
    final String snippet = MINIMAL_HEADER;

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0 );
    assertValues( dataSet, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileMissingSampleRateFail() throws Exception
  {
    final String snippet = ";Channels: 1\n0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0 );
    assertValues( dataSet, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadDataFileSizeMismatchFail() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 2\n0@0\n1@1\n3@3";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0 );
    assertValues( dataSet, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileWithAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n;AbsoluteLength: 9\n0@0\n1@1\n3@3";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 1, 3, 9 );
    assertValues( dataSet, 0, 1, 3, 3 );
    assertAbsoluteLength( dataSet, 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadDataFileWithInvalidAbsoluteLengthOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 3\n;AbsoluteLength: 2\n0@0\n1@1\n3@3";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 1, 3 );
    assertValues( dataSet, 0, 1, 3 );
    assertAbsoluteLength( dataSet, 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadFileSetOk() throws Exception
  {
    for ( File dataFile : this.dataFiles )
    {
      final FileReader reader = new FileReader( dataFile );
      try
      {
        DataSet dataSet = OlsDataHelper.read( reader );

        assertNotNull( "Failed to read: " + dataFile.getName(), dataSet.getCapturedData() );
      }
      finally
      {
        reader.close();
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

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, Integer.MIN_VALUE, Integer.MIN_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test( expected = IOException.class )
  public void testReadInvalidTimeValueOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@" + Long.MIN_VALUE;

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0 );
    assertValues( dataSet, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxSampleValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        Integer.toHexString( Integer.MAX_VALUE ) + "@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, Integer.MAX_VALUE, Integer.MAX_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxSampleValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + Integer.toHexString( Integer.MAX_VALUE ) + "@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, Integer.MAX_VALUE, Integer.MAX_VALUE );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxTimeValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        "0@" + Long.MAX_VALUE;

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, Long.MAX_VALUE, Long.MAX_VALUE );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMaxTimeValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@" + Long.MAX_VALUE;

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, Long.MAX_VALUE, Long.MAX_VALUE );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinimalDataFileOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinSampleValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinSampleValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinTimeValueWithMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Size: 1\n" + //
        "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMinTimeValueWithoutMetadataOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 0 );
    assertValues( dataSet, 0, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadMSBDataOk() throws Exception
  {
    // Issue #80: MSB data isn't correctly read back from stored data...
    final String snippet = MINIMAL_HEADER + "80000000@0\n00000000@1\n80000000@2\n";

    DataSet dataSet = readOlsData( snippet );

    assertTimeStamps( dataSet, 0, 1, 2 );
    assertValues( dataSet, 0x80000000, 0x0, 0x80000000 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadNegativeEnabledChannelsOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";EnabledChannels: " + 0xFF000000 + "\n" + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertChannelGroupDisabled( dataSet, 0 );
    assertChannelGroupDisabled( dataSet, 1 );
    assertChannelGroupDisabled( dataSet, 2 );
    assertChannelGroupEnabled( dataSet, 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadOldUnsetCursorOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";CursorA: " + Long.MIN_VALUE + "\n" + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertCursorUnset( dataSet, 0 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadPositiveEnabledChannelsOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";EnabledChannels: " + 0x00FF0000 + "\n" + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertChannelGroupDisabled( dataSet, 0 );
    assertChannelGroupDisabled( dataSet, 1 );
    assertChannelGroupEnabled( dataSet, 2 );
    assertChannelGroupDisabled( dataSet, 3 );
  }

  /**
   * Test method for {@link OlsDataHelper#read(Project, Reader)}.
   */
  @Test
  public void testReadUnsetCursorOk() throws Exception
  {
    final String snippet = MINIMAL_HEADER + ";Cursor9: " + Long.MIN_VALUE + "\n" + "0@0";

    DataSet dataSet = readOlsData( snippet );

    assertCursorUnset( dataSet, 9 );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testSimpleWriteOk() throws Exception
  {
    DataSetImpl dataSet = new DataSetImpl( new CapturedData( new int[] { 1 }, new long[] { 2L }, -1, 100, 2, 2, 1 ),
        new DataSetImpl(), false );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( dataSet, writer );

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
    DataSetImpl dataSet = new DataSetImpl( new CapturedData( new int[] { -1 }, new long[] { 1L }, -1, 100, 2, 2, 1 ),
        new DataSetImpl(), false );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( dataSet, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( "ffffffff@1" ) );
  }

  /**
   * Test method for {@link OlsDataHelper#write(Project, Writer)}.
   */
  @Test
  public void testWriteInvalidTimestampOk() throws Exception
  {
    DataSetImpl dataSet = new DataSetImpl( new CapturedData( new int[] { 1 }, new long[] { -1L }, -1, 100, 2, 2, 1 ),
        new DataSetImpl(), false );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( dataSet, writer );

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
    DataSetImpl dataSet = new DataSetImpl( new CapturedData( new int[] { 0x80000000, 0x0, 0x80000000 }, new long[] {
        0L, 1L, 2L }, -1, 100, 2, 2, 1 ), new DataSetImpl(), false );

    final StringWriter writer = new StringWriter();
    OlsDataHelper.write( dataSet, writer );

    final String snippet = writer.toString();
    assertTrue( snippet.contains( "80000000@0" ) );
    assertTrue( snippet.contains( "00000000@1" ) );
    assertTrue( snippet.contains( "80000000@2" ) );
  }

  private void assertAbsoluteLength( DataSet aDataSet, final long aAbsLength )
  {
    AcquisitionResult data = aDataSet.getCapturedData();
    assertNotNull( data );

    final long absLength = data.getAbsoluteLength();
    assertEquals( aAbsLength, absLength );
  }

  private void assertChannelGroupDisabled( DataSet aDataSet, final int aGroupIdx )
  {
    AcquisitionResult data = aDataSet.getCapturedData();
    assertNotNull( data );

    assertTrue( ( data.getEnabledChannels() & ( 0xFFL << ( aGroupIdx * 8 ) ) ) == 0 );
  }

  private void assertChannelGroupEnabled( DataSet aDataSet, final int aGroupIdx )
  {
    AcquisitionResult data = aDataSet.getCapturedData();
    assertNotNull( data );

    assertTrue( ( data.getEnabledChannels() & ( 0xFFL << ( aGroupIdx * 8 ) ) ) != 0 );
  }

  private void assertCursorSet( DataSet aDataSet, final int aCursorIdx, final long aCursorValue )
  {
    Cursor[] cursors = aDataSet.getCursors();
    assertNotNull( cursors );
    assertTrue( cursors.length > aCursorIdx );
    assertTrue( cursors[aCursorIdx].toString(), cursors[aCursorIdx].isDefined() );
    assertEquals( cursors[aCursorIdx].toString(), aCursorValue, cursors[aCursorIdx].getTimestamp() );
  }

  private void assertCursorUnset( DataSet aDataSet, final int aCursorIdx )
  {
    Cursor[] cursors = aDataSet.getCursors();
    assertNotNull( cursors );
    assertTrue( cursors.length > aCursorIdx );
    assertFalse( cursors[aCursorIdx].toString(), cursors[aCursorIdx].isDefined() );
  }

  private void assertTimeStamps( DataSet aDataSet, final long... aTimestamps )
  {
    AcquisitionResult data = aDataSet.getCapturedData();
    assertNotNull( data );

    assertArrayEquals( aTimestamps, data.getTimestamps() );
  }

  private void assertValues( DataSet aDataSet, final int... aValues )
  {
    AcquisitionResult data = aDataSet.getCapturedData();
    assertNotNull( data );

    assertArrayEquals( aValues, data.getValues() );
  }

  private DataSet readOlsData( final String text ) throws IOException
  {
    return OlsDataHelper.read( new StringReader( text ) );
  }
}
