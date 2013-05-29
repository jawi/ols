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

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialDecoderCallback;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitOrder;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitEncoding;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitLevel;
import nl.lxtreme.ols.tool.uart.impl.*;

import org.junit.*;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.*;


/**
 * (Parameterized) tests cases for {@link UARTAnalyserTask}.
 */
public class ISO7816DataTest
{
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
            "sy-card-atr-rxd-channel1-baud9600-parity-odd.ols",
            9600, /* baudrate */
            0, /* start timestamp */
            762348, /* end timestamp */
            1, /* RxD */
            Parity.EVEN, /* parity */
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
    while ( dataIter.hasNext() )
    {
      final UARTData data = dataIter.next();
      if ( data.isEvent() )
      {
        errors++;
      }
      else if ( symbolIdx < aExpectedData.length )
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
   * {@link nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder#decodeDataLine(AcquisitionResult, int, SerialDecoderCallback)}
   * .
   */
  @Test
  public void testDirectConventionOk() throws Exception
  {
    UARTDataSet result = analyseDataFile( "sy-card-atr-rxd-channel1-baud9600-parity-odd.ols", 9600, Parity.EVEN,
        1 /* RxD */, true /* inverse convention */);

    assertEquals( 22, result.getDecodedSymbols() );
    assertEquals( 2, result.getDetectedErrors() );
    assertDataEvents( result, new int[] { 0x3f, 0xfd, 0x11, 0x25, 0x02, 0x50, 0x00, 0x03, 0x33, 0xb0, 0x15, 0x69, 0xff,
        0x4a, 0x50, 0xf0, 0x80, 0x03, 0x4b, 0x4c, 0x03, 0xff } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder#decodeDataLine(AcquisitionResult, int, SerialDecoderCallback)}
   * .
   */
  @Test
  public void testInverseConventionOk() throws Exception
  {
    UARTDataSet result = analyseDataFile( "openpgp-card-atr-rxd-channel1-baud9600-parity-even.ols", 9600, Parity.EVEN,
        1 /* RxD */, false /* inverse convention */);

    assertEquals( 21, result.getDecodedSymbols() );
    assertEquals( 1, result.getDetectedErrors() );
    assertDataEvents( result, new int[] { 0x3b, 0xfa, 0x13, 0x00, 0xff, 0x81, 0x31, 0x80, 0x45, 0x00, 0x31, 0xc1, 0x73,
        0xc0, 0x01, 0x00, 0x00, 0x90, 0x00, 0xb1, 0x00 } );
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
  private UARTDataSet analyseDataFile( final String aResourceName, final int aBaudRate, final Parity aParity,
      final int aRxDChannel, final boolean aInverseConvention ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener tpl = Mockito.mock( ToolProgressListener.class );
    AnnotationListener al = Mockito.mock( AnnotationListener.class );

    UARTAnalyserTask worker = new UARTAnalyserTask( toolContext, tpl, al );
    worker.setBitCount( 8 );
    worker.setStopBits( StopBits.ONE );
    worker.setParity( aParity );
    worker.setBaudRate( aBaudRate );
    worker.setRxdIndex( aRxDChannel );
    worker.setTxdIndex( -1 );
    if ( aInverseConvention )
    {
      worker.setBitOrder( BitOrder.MSB_FIRST );
      worker.setBitEncoding ( BitEncoding.HIGH_IS_SPACE );
    }
    else
    {
      worker.setBitOrder( BitOrder.LSB_FIRST );
      worker.setBitEncoding( BitEncoding.HIGH_IS_MARK );
    }
    worker.setIdleLevel( BitLevel.HIGH );

    UARTDataSet result = worker.call();
    assertNotNull( result );
    return result;
  }
}
