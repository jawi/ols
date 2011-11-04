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


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.*;


/**
 * (Parameterized) tests cases for {@link UARTAnalyserTask}.
 */
@RunWith( Parameterized.class )
public class UARTAnalyserWorkerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int expectedErrorCount;
  private final int expectedSymbolCount;
  private final int expectedBaudrate;
  private final int[] channels;

  // CONSTRUCTORS

  /**
   * Creates a new UARTAnalyserWorkerTest instance.
   */
  public UARTAnalyserWorkerDataFilesTest( final String aResourceName, final int aExpectedErrorCount,
      final int aExpectedSymbolCount, final int aExpectedBaudrate, final int... aChannels )
  {
    this.resourceName = aResourceName;
    this.expectedErrorCount = aExpectedErrorCount;
    this.expectedSymbolCount = aExpectedSymbolCount;
    this.expectedBaudrate = aExpectedBaudrate;
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
        // { filename, error count, symbol count, baudrate, (rxd, txd) }
            { "uart_8bit_1.ols", 0, 33, 38400, new int[] { 0, -1 } }, //
            { "uart_8bit_2.ols", 0, 6, 9600, new int[] { 2, -1 } }, //
            { "uart_8bit_3.ols", 50, 418, 9600, new int[] { 1, 0 } }, //
            { "uart_8bit_4_38400bps.ols", 0, 22, 38400, new int[] { 0, -1 } }, //
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.UARTAnalyserTask#doInBackground()}.
   */
  @Test
  public void testUartAnalysisOk() throws Exception
  {
    UARTDataSet result = analyseDataFile( this.resourceName );
    assertEquals( this.expectedErrorCount, result.getDetectedErrors() );
    assertEquals( this.expectedSymbolCount, result.getDecodedSymbols() );
    assertEquals( this.expectedBaudrate, result.getBaudRate() );
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
  private UARTDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    DataContainer container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener tpl = Mockito.mock( ToolProgressListener.class );
    AnnotationListener al = Mockito.mock( AnnotationListener.class );

    UARTAnalyserTask worker = new UARTAnalyserTask( toolContext, tpl, al );
    worker.setStopBits( UARTStopBits.STOP_1 );
    worker.setParity( UARTParity.NONE );
    worker.setBitCount( 8 );
    worker.setRxdIndex( this.channels[0] );
    worker.setTxdIndex( this.channels[1] );

    UARTDataSet result = worker.call();
    assertNotNull( result );
    return result;
  }
}
