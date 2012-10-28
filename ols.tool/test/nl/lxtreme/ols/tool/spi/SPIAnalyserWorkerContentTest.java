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
import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * @author jawi
 */
public class SPIAnalyserWorkerContentTest extends TestCase
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.spi.SPIAnalyserTask#doInBackground()}.
   */
  public void testAnalyzeDataFile1() throws Exception
  {
    AnnotationCollector result = analyseDataFile( "spi_8bit_4.ols", 8, SPIMode.MODE_0, BitOrder.MSB_FIRST,
        false /* aHonourCS */, -1 /* miso */, 1 /* mosi */, 0 /* cs */, 3 /* sck */);

    result.assertDataSymbolsOn( 1, 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E',
        'S', 'T', ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S', 'T',
        ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S', 'T', ' ', 'T', 'E', 'S' );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.spi.SPIAnalyserTask#doInBackground()}.
   */
  public void testAnalyzeDataFile2() throws Exception
  {
    AnnotationCollector result = analyseDataFile( "spi_8bit_1.ols", 8, SPIMode.MODE_2, BitOrder.LSB_FIRST,
        true /* aHonourCS */, -1 /* miso */, 1 /* mosi */, 3 /* cs */, 2 /* sck */);

    result.assertDataSymbolsOn( 1, 147, 0, 0, 211, 192, 1, 193, 8, 166, 177, 176 );
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
