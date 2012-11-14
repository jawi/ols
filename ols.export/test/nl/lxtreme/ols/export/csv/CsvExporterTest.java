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


import static org.mockito.Mockito.*;

import java.io.*;
import javax.swing.*;

import junit.framework.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.testutil.DataTestUtils.TestDataProvider;


/**
 * Provides test cases for {@link CsvExporter}.
 */
public class CsvExporterTest extends TestCase
{
  // CONSTANTS

  private static final int CHANNEL_COUNT = 4;
  private static final int SAMPLE_RATE = 100000;

  // VARIABLES

  private JComponent component;
  private ByteArrayOutputStream outputStream;
  private CsvExporter exporter;
  private String separator;

  // METHODS

  /**
   * Test method for
   * {@link CsvExporter#export(DataSet, JComponent, OutputStream)}.
   * <p>
   * Tests the export of a 4-bit data set without a sample rate and a trigger
   * position, works correctly.
   * </p>
   */
  public void testExportDataSetWithoutSampleRateAndTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = -1;
    final long triggerPos = -1;

    final AcquisitionData data = generateAcquisitionData( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( data, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */+ dataSize;
    int expectedCols = 1 /* time */+ channelCount;
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
  public void testExportDataSetWithoutSampleRateButWithTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = -1;
    final long triggerPos = 10;

    final AcquisitionData data = generateAcquisitionData( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( data, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */+ dataSize;
    int expectedCols = 2 /* state abs, state rel */+ channelCount;
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
  public void testExportDataSetWithSampleRateAndTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = SAMPLE_RATE;
    final long triggerPos = 10;

    final AcquisitionData data = generateAcquisitionData( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( data, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */+ dataSize;
    int expectedCols = 3 /* time abs, time rel & sample rate */+ channelCount;
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
  public void testExportDataSetWithSampleRateButWithoutTriggerOk() throws Exception
  {
    final int channelCount = CHANNEL_COUNT;
    final int dataSize = 10;
    final int sampleRate = SAMPLE_RATE;
    final long triggerPos = -1;

    final AcquisitionData data = generateAcquisitionData( channelCount, dataSize, sampleRate, triggerPos );

    this.exporter.export( data, this.component, this.outputStream );

    String[] results = getCsvData();
    assertNotNull( results );

    int expectedRows = 1 /* header */+ dataSize;
    int expectedCols = 2 /* time + samplerate */+ channelCount;
    assertCsvDimensions( results, expectedRows, expectedCols );
  }

  /**
   * Set up for each test case.
   */
  @Override
  protected void setUp()
  {
    this.component = mock( JComponent.class );
    this.outputStream = new ByteArrayOutputStream();
    this.exporter = spy( new CsvExporter() );
    this.separator = System.getProperty( "line.separator", "\n" );
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
      String[] csvColumns = getCsvCols( csvDataRow );
      assertEquals( "Column count mismatch for: " + csvDataRow, aExpectedCols, csvColumns.length );
    }
  }

  /**
   * Generates acquisition data for use in the test cases.
   */
  private AcquisitionData generateAcquisitionData( final int aChannelCount, final int aSize, final int aSampleRate,
      final long aTriggerPos )
  {
    return DataTestUtils.generateAcquisitionData( aSize, aChannelCount, aSampleRate, aTriggerPos,
        new TestDataProvider()
        {
          @Override
          public void fillData( final int[] aValues, final long[] aTimestamps, final int aDataSize )
          {
            int mask = ( int )( ( 1L << aChannelCount ) - 1L );
            for ( int i = 0, value = 0; i < aSize; i++, value++ )
            {
              aValues[i] = value & mask;
              aTimestamps[i] = value;
              value++;
            }
          }
        } );
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
