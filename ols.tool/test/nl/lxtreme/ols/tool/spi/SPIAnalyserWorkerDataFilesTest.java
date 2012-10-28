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


import java.net.*;
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * (Parameterized) tests cases for {@link SPIAnalyserTask}.
 */
public class SPIAnalyserWorkerDataFilesTest extends TestCase
{
  // METHODS

  /**
   * @return a collection of test data.
   */
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // { filename, datagram size (bits), MiSo symbol count, MoSi symbol
        // count, (MISO, MOSI, CS, SCLK) }
            { "spi_8bit_1.ols", 8, 11, 11, SPIMode.MODE_2, BitOrder.MSB_FIRST, true, new int[] { 0, 1, 3, 2 } }, //
            { "spi_8bit_2.ols", 8, 195, 195, SPIMode.MODE_2, BitOrder.MSB_FIRST, true, new int[] { 0, 1, 3, 2 } }, //
            { "spi_9bit_3.ols", 9, 17, 17, SPIMode.MODE_2, BitOrder.LSB_FIRST, true, new int[] { 0, 3, 1, 2 } }, //
            { "spi_8bit_4.ols", 8, 0, 53, SPIMode.MODE_0, BitOrder.MSB_FIRST, false, new int[] { -1, 1, 0, 3 } }, //
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.spi.SPIAnalyserTask#doInBackground()}.
   */
  @SuppressWarnings( "boxing" )
  public void testAnalyzeDataFile() throws Exception
  {
    for ( Object[] params : getTestData() )
    {
      String resource = ( String )params[0];
      int bitCount = ( Integer )params[1];
      int misoCount = ( Integer )params[2];
      int mosiCount = ( Integer )params[3];
      SPIMode mode = ( SPIMode )params[4];
      BitOrder order = ( BitOrder )params[5];
      boolean honourCS = ( Boolean )params[6];
      int[] channels = ( int[] )params[7];

      AnnotationCollector result = analyseDataFile( resource, bitCount, mode, order, honourCS, channels );

      assertEquals( "Data file: " + resource + ", MISO count mismatch", misoCount,
          result.countSymbolsOn( channels[0] /* miso */) );
      assertEquals( "Data file: " + resource + ", MOSI count mismatch", mosiCount,
          result.countSymbolsOn( channels[1] /* mosi */) );
    }
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aBitCount, final SPIMode aMode,
      final BitOrder aBitOrder, final boolean aHonourCS, final int... aChannels ) throws Exception
  {
    AnnotationCollector result = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, result );

    Configuration config = new MapConfiguration();
    config.asMap().put( "protocol", "STANDARD" );
    config.asMap().put( "bitCount", Integer.valueOf( aBitCount ) );
    config.asMap().put( "honourCS", Boolean.valueOf( aHonourCS ) );
    config.asMap().put( "reportCS", Boolean.FALSE );
    config.asMap().put( "mode", aMode );
    config.asMap().put( "bitOrder", aBitOrder );
    if ( aChannels.length > 0 )
    {
      config.asMap().put( "misoIdx", Integer.valueOf( aChannels[0] ) );
    }
    if ( aChannels.length > 1 )
    {
      config.asMap().put( "mosiIdx", Integer.valueOf( aChannels[1] ) );
    }
    if ( aChannels.length > 2 )
    {
      config.asMap().put( "csIdx", Integer.valueOf( aChannels[2] ) );
    }
    if ( aChannels.length > 3 )
    {
      config.asMap().put( "sckIdx", Integer.valueOf( aChannels[3] ) );
    }

    SPIAnalyserTask worker = new SPIAnalyserTask( toolContext, config );
    worker.call();

    return result;
  }
}
