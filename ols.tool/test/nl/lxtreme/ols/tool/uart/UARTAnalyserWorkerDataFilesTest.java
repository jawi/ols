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

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitOrder;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitEncoding;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitLevel;
import nl.lxtreme.ols.tool.uart.impl.*;

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
  private final int baudrate;
  private final int expectedErrorCount;
  private final int expectedSymbolCount;
  private final int expectedBaudrate;
  private final Parity parity;
  private final int[] channels;

  // CONSTRUCTORS

  /**
   * Creates a new UARTAnalyserWorkerTest instance.
   */
  public UARTAnalyserWorkerDataFilesTest( final String aResourceName, final int aExpectedErrorCount,
      final int aExpectedSymbolCount, final int aBaudrate, final int aExpectedBaudrate, final Parity aParity,
      final int... aChannels )
  {
    this.resourceName = aResourceName;
    this.baudrate = aBaudrate;
    this.expectedErrorCount = aExpectedErrorCount;
    this.expectedSymbolCount = aExpectedSymbolCount;
    this.expectedBaudrate = aExpectedBaudrate;
    this.parity = aParity;
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
            { "uart_8bit_1.ols", 0, 33, -1, 38400, Parity.NONE, new int[] { 0, -1 } }, // 0
            { "uart_8bit_2.ols", 0, 6, -1, 9600, Parity.NONE, new int[] { 2, -1 } }, // 1
            { "uart_8bit_3.ols", 48, 419, -1, 9600, Parity.NONE, new int[] { 1, 0 } }, // 2
            { "uart_8bit_4_38400bps.ols", 0, 22, -1, 38400, Parity.NONE, new int[] { 0, -1 } }, // 3
            { "uart_8bit_5_115200bps.ols", 0, 306, 115200, 115200, Parity.NONE, new int[] { 0, -1 } }, // 4
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.impl.UARTAnalyserTask#doInBackground()}.
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
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener tpl = Mockito.mock( ToolProgressListener.class );
    AnnotationListener al = Mockito.mock( AnnotationListener.class );

    UARTAnalyserTask worker = new UARTAnalyserTask( toolContext, tpl, al );
    worker.setStopBits( StopBits.ONE );
    worker.setBitCount( 8 );
    worker.setParity( this.parity );
    worker.setBaudRate( this.baudrate );
    worker.setRxdIndex( this.channels[0] );
    worker.setTxdIndex( this.channels[1] );
    worker.setBitOrder( BitOrder.LSB_FIRST );
    worker.setBitEncoding( BitEncoding.HIGH_IS_MARK );
    worker.setIdleLevel( BitLevel.HIGH );

    UARTDataSet result = worker.call();
    assertNotNull( result );
    return result;
  }
}
