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


import java.net.*;
import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialDecoderCallback;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
import nl.lxtreme.ols.tool.uart.impl.*;


/**
 * (Parameterized) tests cases for {@link UARTAnalyserTask}.
 */
public class ISO7816DataTest extends TestCase
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder#decodeDataLine(AcquisitionResult, int, SerialDecoderCallback)}
   * .
   */
  public void testDirectConventionOk() throws Exception
  {
    AnnotationCollector result = analyseDataFile( "sy-card-atr-rxd-channel1-baud9600-parity-odd.ols", 9600,
        Parity.EVEN, 1 /* RxD */, true /* inverse convention */);

    assertEquals( 22, result.countSymbolsOn( 1 ) );
    assertEquals( 2, result.countDataErrors() );
    result.assertDataSymbolsOn( 1, new int[] { 0x3f, 0xfd, 0x11, 0x25, 0x02, 0x50, 0x00, 0x03, 0x33, 0xb0, 0x15, 0x69,
        0xff, 0x4a, 0x50, 0xf0, 0x80, 0x03, 0x4b, 0x4c, 0x03, 0xFF } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder#decodeDataLine(AcquisitionResult, int, SerialDecoderCallback)}
   * .
   */
  public void testInverseConventionOk() throws Exception
  {
    AnnotationCollector result = analyseDataFile( "openpgp-card-atr-rxd-channel1-baud9600-parity-even.ols", 9600,
        Parity.EVEN, 1 /* RxD */, false /* inverse convention */);

    assertEquals( 21, result.countSymbolsOn( 1 ) );
    assertEquals( 1, result.countDataErrors() );
    result.assertDataSymbolsOn( 1, new int[] { 0x3b, 0xfa, 0x13, 0x00, 0xff, 0x81, 0x31, 0x80, 0x45, 0x00, 0x31, 0xc1,
        0x73, 0xc0, 0x01, 0x00, 0x00, 0x90, 0x00, 0xb1, 0x00 } );
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aBaudRate, final Parity aParity,
      final int aRxDChannel, final boolean aInverseConvention ) throws Exception
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
    config.asMap().put( "rxdIdx", Integer.valueOf( aRxDChannel ) );
    config.asMap().put( "inverted", Boolean.valueOf( aInverseConvention ) );
    config.asMap().put( "msbFirst", Boolean.valueOf( aInverseConvention ) );

    UARTAnalyserTask worker = new UARTAnalyserTask( toolContext, config );
    worker.call();

    return result;
  }
}
