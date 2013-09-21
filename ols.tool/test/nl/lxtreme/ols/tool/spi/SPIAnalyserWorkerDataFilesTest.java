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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.spi;


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.util.NumberUtils.BitOrder;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.*;


/**
 * (Parameterized) tests cases for {@link SPIAnalyserTask}.
 */
@RunWith( Parameterized.class )
public class SPIAnalyserWorkerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int bitCount;
  private final int expectedMisoSymbolCount;
  private final int expectedMosiSymbolCount;
  private final SPIMode spiMode;
  private final BitOrder bitOrder;
  private final int[] channels;
  private final boolean honourCS;

  // CONSTRUCTORS

  /**
   * Creates a new SPIAnalyserWorkerDataFilesTest instance.
   */
  public SPIAnalyserWorkerDataFilesTest( final String aResourceName, final int aBitCount,
      final int aExpectedMisoSymbolCount, final int aExpectedMosiSymbolCount, final SPIMode aSPIMode,
      final BitOrder aBitOrder, final boolean aHonourCS, final int[] aChannels )
  {
    this.resourceName = aResourceName;
    this.bitCount = aBitCount;
    this.expectedMisoSymbolCount = aExpectedMisoSymbolCount;
    this.expectedMosiSymbolCount = aExpectedMosiSymbolCount;
    this.spiMode = aSPIMode;
    this.bitOrder = aBitOrder;
    this.honourCS = aHonourCS;
    this.channels = aChannels;
  }

  // METHODS

  /**
   * @return a collection of test data.
   */
  @Parameters
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // { filename, datagram size (bits), MiSo symbol count, MoSi symbol
        // count, (MISO, MOSI, CS, SCLK) }
            { "spi_8bit_1.ols", 8, 7, 7, SPIMode.MODE_2, BitOrder.MSB_FIRST, true, new int[] { 0, 1, 3, 2 } }, //
            { "spi_8bit_2.ols", 8, 195, 195, SPIMode.MODE_2, BitOrder.MSB_FIRST, true, new int[] { 0, 1, 3, 2 } }, //
            { "spi_9bit_3.ols", 9, 17, 17, SPIMode.MODE_2, BitOrder.LSB_FIRST, true, new int[] { 0, 3, 1, 2 } }, //
            { "spi_8bit_4.ols", 8, 0, 53, SPIMode.MODE_0, BitOrder.MSB_FIRST, false, new int[] { -1, 1, 0, 3 } }, //
        } );
  }

  /**
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private static void assertEventCount( final SPIDataSet aDataSet, final String aEventName,
      final int aExpectedEventCount )
  {
    int count = 0;
    for ( SPIData data : aDataSet.getData() )
    {
      if ( aEventName.equals( data.getDataName() ) )
      {
        count++;
      }
    }
    assertEquals( "Not all events were seen?!", aExpectedEventCount, count );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.spi.SPIAnalyserTask#doInBackground()}.
   */
  @Test
  public void testAnalyzeDataFile() throws Exception
  {
    SPIDataSet result = analyseDataFile( this.resourceName );
    assertEventCount( result, SPIDataSet.SPI_MISO, this.expectedMisoSymbolCount );
    assertEventCount( result, SPIDataSet.SPI_MOSI, this.expectedMosiSymbolCount );
  }

  /**
   * Analyses the data file identified by the given resource name.
   * 
   * @param aResourceName
   *          the name of the resource (= data file) to analyse, cannot be
   *          <code>null</code>.
   * @return the analysis results, never <code>null</code>.
   * @throws Exception
   *           in case of exceptions.
   */
  private SPIDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener tpl = Mockito.mock( ToolProgressListener.class );
    AnnotationListener al = Mockito.mock( AnnotationListener.class );

    SPIAnalyserTask worker = new SPIAnalyserTask( toolContext, tpl, al );
    worker.setBitCount( this.bitCount - 1 );
    worker.setHonourCS( this.honourCS );
    worker.setReportCS( false );
    worker.setProtocol( SPIFIMode.STANDARD );
    worker.setSPIMode( this.spiMode );
    worker.setOrder( this.bitOrder );
    worker.setIO0Index( this.channels[1] );
    worker.setIO1Index( this.channels[0] );
    worker.setCSIndex( this.channels[2] );
    worker.setSCKIndex( this.channels[3] );

    SPIDataSet result = worker.call();
    assertNotNull( result );
    return result;
  }

}
