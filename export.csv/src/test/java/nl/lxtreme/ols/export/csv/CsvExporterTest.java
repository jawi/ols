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
package nl.lxtreme.ols.export.csv;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.test.data.project.*;

import org.junit.*;
import org.junit.rules.*;


/**
 * Provides test cases for {@link CsvExporter}.
 */
public class CsvExporterTest
{
  // CONSTANTS

  private static final int CHANNEL_COUNT = 4;
  private static final int SAMPLE_RATE = 100000;

  // VARIABLES

  private JComponent component;
  private ByteArrayOutputStream outputStream;
  private CsvExporter exporter;
  private String separator;

  @Rule
  public TemporaryFolder folder = new TemporaryFolder();

  // METHODS

  /**
   * Set up for each test case.
   */
  @Before
  public void setUp()
  {
    this.component = mock( JComponent.class );
    this.outputStream = new ByteArrayOutputStream();
    this.exporter = spy( new CsvExporter() );
    this.separator = System.getProperty( "line.separator", "\n" );
  }

  /**
   * Test method for
   * {@link CsvExporter#export(DataSet, JComponent, OutputStream)}.
   * <p>
   * Tests issue #244 - export with null channels (due to enabledMask not being
   * continuous).
   * </p>
   */
  @Test
  public void testExportDataSetWithMaskedOutChannels() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = -1;
    final long triggerPos = -1;

    // create a dataset with only channels 0 and 3 enabled...
    final DataSet dataSet = createTestDataSet( channelCount, dataSize, sampleRate, triggerPos, 9 );

    this.exporter.export( dataSet, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */ + dataSize;
    int expectedCols = 1 /* time */ + 2 /* channels 0 & 3 */;
    assertCsvDimensions( results, expectedRows, expectedCols );
  }

  /**
   * Test method for
   * {@link CsvExporter#export(DataSet, JComponent, OutputStream)}.
   * <p>
   * Tests the export of a 4-bit data set without a sample rate and a trigger
   * position, works correctly.
   * </p>
   */
  @Test
  public void testExportDataSetWithoutSampleRateAndTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = -1;
    final long triggerPos = -1;

    final DataSet dataSet = createTestDataSet( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( dataSet, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */ + dataSize;
    int expectedCols = 1 /* time */ + channelCount;
    assertCsvDimensions( results, expectedRows, expectedCols );
  }

  /**
   * Test method for
   * {@link CsvExporter#export(DataSet, JComponent, OutputStream)}.
   * <p>
   * Tests the export of a 4-bit data set without a sample rate, but with a
   * trigger position, works correctly.
   * </p>
   */
  @Test
  public void testExportDataSetWithoutSampleRateButWithTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = -1;
    final long triggerPos = 10;

    final DataSet dataSet = createTestDataSet( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( dataSet, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */ + dataSize;
    int expectedCols = 2 /* state abs, state rel */ + channelCount;
    assertCsvDimensions( results, expectedRows, expectedCols );
  }

  /**
   * Test method for
   * {@link CsvExporter#export(DataSet, JComponent, OutputStream)}.
   * <p>
   * Tests the export of a 4-bit data set with a sample rate and a trigger
   * position, works correctly.
   * </p>
   */
  @Test
  public void testExportDataSetWithSampleRateAndTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = SAMPLE_RATE;
    final long triggerPos = 10;

    final DataSet dataSet = createTestDataSet( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( dataSet, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */ + dataSize;
    int expectedCols = 3 /* time abs, time rel & sample rate */ + channelCount;
    assertCsvDimensions( results, expectedRows, expectedCols );
  }

  /**
   * Test method for
   * {@link CsvExporter#export(DataSet, JComponent, OutputStream)}.
   * <p>
   * Tests the export of a 4-bit data set with a sample rate, but without a
   * trigger position, works correctly.
   * </p>
   */
  @Test
  public void testExportDataSetWithSampleRateButWithoutTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = SAMPLE_RATE;
    final long triggerPos = -1;

    final DataSet dataSet = createTestDataSet( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( dataSet, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */ + dataSize;
    int expectedCols = 2 /* time + samplerate */ + channelCount;
    assertCsvDimensions( results, expectedRows, expectedCols );
  }

  /**
   * @param aCsvData
   * @param aExpectedRows
   * @param aExpectedCols
   */
  private void assertCsvDimensions( final String[] aCsvData, final int aExpectedRows, final int aExpectedCols )
  {
    assertEquals( aExpectedRows, aCsvData.length );

    for ( String csvDataRow : aCsvData )
    {
      assertEquals( "Column count mismatch for: " + csvDataRow, aExpectedCols, getCsvCols( csvDataRow ).length );
    }
  }

  private DataSet createTestDataSet( final int aChannelCount, final int aSize, final int aSampleRate,
      final long aTriggerPos )
  {
    return createTestDataSet( aChannelCount, aSize, aSampleRate, aTriggerPos, ( 1 << aChannelCount ) - 1 );
  }

  private DataSet createTestDataSet( final int aChannelCount, final int aSize, final int aSampleRate,
      final long aTriggerPos, final long aMask )
  {
    List<Integer> values = new ArrayList<Integer>( aSize );
    List<Long> timestamps = new ArrayList<Long>( aSize );

    int value = 0;
    int mask = ( int )( aMask & 0xffffffff );
    for ( int i = 0; i < aSize; i++ )
    {
      values.add( Integer.valueOf( value & mask ) );
      timestamps.add( Long.valueOf( value ) );
      value++;
    }

    CapturedData capData = new CapturedData( values, timestamps, aTriggerPos, aSampleRate, aChannelCount, mask,
        value - 1 );

    StubDataSet dataSet = new StubDataSet();
    dataSet.setCapturedData( capData );
    dataSet.setCursorsEnabled( false );
    return dataSet;
  }

  /**
   * @param aCsvRow
   * @return
   */
  private String[] getCsvCols( final String aCsvRow )
  {
    return aCsvRow.split( "," );
  }

  /**
   * @return
   */
  private String[] getCsvData()
  {
    String result = this.outputStream.toString();
    assertNotNull( result );
    return result.split( this.separator );
  }
}
