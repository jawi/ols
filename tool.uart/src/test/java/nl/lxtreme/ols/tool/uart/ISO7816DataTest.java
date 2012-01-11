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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.uart;


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.*;


/**
 * (Parameterized) tests cases for {@link UARTAnalyserTask}.
 */
@Ignore
@RunWith( Parameterized.class )
public class ISO7816DataTest
{
  // VARIABLES

  private final String resourceName;
  private final int baudrate;
  private final long cursorA;
  private final long cursorB;
  private final int[] channels;
  private final int[] symbols;

  // CONSTRUCTORS

  /**
   * Creates a new ISO7816DataTest instance.
   */
  public ISO7816DataTest( final String aResourceName, final int aBaudrate, final long aCursorA, final long aCursorB,
      final int[] aChannels, final int[] aSymbols )
  {
    this.resourceName = aResourceName;
    this.baudrate = aBaudrate;
    this.cursorA = aCursorA;
    this.cursorB = aCursorB;
    this.channels = aChannels;
    this.symbols = aSymbols;
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
        {
            "uart_iso7816_kd1.ols",
            12096, /* baudrate */
            59331, /* start timestamp */
            762348, /* end timestamp */
            new int[] { 2, -1 }, /* { RxD, TxD } */
            /* expected symbols */
            new int[] { 0x3f, 0xfd, 0xfc, 0x15, 0x25, 0x02, 0x50, 0xff, 0x00, 0x03, 0xff, 0x33, 0xb0, 0x15, 0x69, 0xff,
                0x4a, 0x50, 0xf0, 0x80, 0x03, 0x4b, 0x4c, 0x03, 0x00 } //
        }, //
        } );
  }

  /**
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private static void assertDataEvents( final UARTDataSet aDataSet, final int... aExpectedData )
  {
    final Iterator<UARTData> dataIter = aDataSet.getData().iterator();

    int symbolIdx = 0, errors = 0;
    while ( dataIter.hasNext() && ( symbolIdx < aExpectedData.length ) )
    {
      final UARTData data = dataIter.next();
      if ( data.isEvent() )
      {
        errors++;
      }
      else
      {
        assertEquals( "Data value at index " + symbolIdx + " not equal, ", aExpectedData[symbolIdx], data.getData() );
        symbolIdx++;
      }
    }
    assertEquals( "Not all data events were seen?!", aExpectedData.length, symbolIdx );
    assertEquals( "Not all symbols were seen?!", symbolIdx, aDataSet.getDecodedSymbols() );
    assertEquals( "Not all errors were seen?!", errors, aDataSet.getDetectedErrors() );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.UARTAnalyserTask#doInBackground()}.
   */
  @Test
  public void testISO7816CompliantDataOk() throws Exception
  {
    UARTDataSet result = analyseDataFile( this.resourceName );
    assertDataEvents( result, this.symbols );
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
    ToolContext toolContext = DataTestUtils.createToolContext( container, this.cursorA, this.cursorB );

    ToolProgressListener tpl = Mockito.mock( ToolProgressListener.class );
    AnnotationListener al = Mockito.mock( AnnotationListener.class );

    UARTAnalyserTask worker = new UARTAnalyserTask( toolContext, tpl, al );
    worker.setStopBits( StopBits.ONE );
    worker.setParity( Parity.EVEN );
    worker.setBitCount( 8 );
    worker.setBaudRate( this.baudrate );
    worker.setRxdIndex( this.channels[0] );
    worker.setTxdIndex( this.channels[1] );
    worker.setInversed( true );
    worker.setInverted( true );

    UARTDataSet result = worker.call();
    assertNotNull( result );
    return result;
  }
}
