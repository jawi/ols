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
package nl.lxtreme.ols.tool.uart;


import java.net.*;
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
import nl.lxtreme.ols.tool.uart.impl.*;
import nl.lxtreme.ols.tool.uart.impl.UARTAnalyserTask.BaudrateAnnotation;


/**
 * (Parameterized) tests cases for {@link UARTAnalyserTask}.
 */
public class UARTAnalyserWorkerDataFilesTest extends TestCase
{
  // METHODS

  /**
   * @return a collection of test data.
   */
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // { filename, error count, symbol count, baudrate, (rxd, txd) }
            { "uart_8bit_1.ols", 0, 33, -1, 38400, Parity.NONE, new int[] { 0, -1 } }, //
            { "uart_8bit_2.ols", 0, 6, -1, 9600, Parity.NONE, new int[] { 2, -1 } }, //
            { "uart_8bit_3.ols", 50, 418, -1, 9600, Parity.NONE, new int[] { 1, 0 } }, //
            { "uart_8bit_4_38400bps.ols", 0, 22, -1, 38400, Parity.NONE, new int[] { 0, -1 } }, //
            { "uart_8bit_5_115200bps.ols", 0, 306, 115200, 115200, Parity.NONE, new int[] { 0, -1 } }, //
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.impl.UARTAnalyserTask#doInBackground()}.
   */
  @SuppressWarnings( "boxing" )
  public void testUartAnalysisOk() throws Exception
  {
    for ( Object[] params : getTestData() )
    {
      String resource = ( String )params[0];
      int errorCount = ( Integer )params[1];
      int symbolCount = ( Integer )params[2];
      int baudrate = ( Integer )params[3];
      int expectedBaudrate = ( Integer )params[4];
      Parity parity = ( Parity )params[5];
      int[] channels = ( int[] )params[6];

      AnnotationCollector result = analyseDataFile( resource, parity, baudrate, channels );

      assertEquals( errorCount, result.countDataErrors() );
      assertEquals( symbolCount, result.countSymbols() );

      BaudrateAnnotation baudrateAnnotation = result.getDataAnnotation( BaudrateAnnotation.class );

      assertEquals( expectedBaudrate, baudrateAnnotation.getProperties().get( "baudrate" ) );
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final Parity aParity, final int aBaudRate,
      final int[] aChannels ) throws Exception
  {
    AnnotationCollector result = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, result );

    Configuration config = new MapConfiguration();
    config.asMap().put( "bitCount", Integer.valueOf( 8 ) );
    config.asMap().put( "stopBits", StopBits.ONE );
    config.asMap().put( "parity", aParity );
    config.asMap().put( "baudrate", Integer.valueOf( aBaudRate ) );
    config.asMap().put( "rxdIdx", Integer.valueOf( aChannels[0] ) );
    config.asMap().put( "txdIdx", Integer.valueOf( aChannels[1] ) );
    config.asMap().put( "inverted", Boolean.FALSE );
    config.asMap().put( "msbFirst", Boolean.FALSE );

    UARTAnalyserTask worker = new UARTAnalyserTask( toolContext, config );
    worker.call();

    return result;
  }
}
