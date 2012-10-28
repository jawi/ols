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
package nl.lxtreme.ols.tool.i2c;


import java.net.*;
import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Tests the decoding of datagrams works correctly.
 */
public class I2CAnalyserWorkerContentTest extends TestCase
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.i2c.I2CAnalyserTask#doInBackground()}.
   */
  public void testKopterDecodingOk() throws Exception
  {
    final AnnotationCollector result = analyseDataFile( "i2c_3.ols", 0, 1 );

    result.assertDataSymbols( 0x52, 0x11, 0x54, 0x10, 0x56, 0x12, 0x58, 0x0B, 0x57, 0x11, 0x52, 0x13, 0x54, 0x11 );
  }

  /**
   * Analyzes the data file identified by the given resource name.
   * 
   * @param aResourceName
   *          the name of the resource (= data file) to analyse, cannot be
   *          <code>null</code>.
   * @return the analysis results, never <code>null</code>.
   * @throws Exception
   *           in case of exceptions.
   */
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aSclIndex, final int aSdaIndex )
      throws Exception
  {
    AnnotationCollector result = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, result );

    Configuration config = new MapConfiguration();
    config.asMap().put( "lineAIdx", Integer.valueOf( aSclIndex ) );
    config.asMap().put( "lineBIdx", Integer.valueOf( aSdaIndex ) );
    config.asMap().put( "detectLines", Boolean.FALSE );
    config.asMap().put( "reportAck", Boolean.FALSE );
    config.asMap().put( "reportNack", Boolean.FALSE );
    config.asMap().put( "reportStart", Boolean.FALSE );
    config.asMap().put( "reportStop", Boolean.FALSE );

    I2CAnalyserTask worker = new I2CAnalyserTask( toolContext, config );
    worker.call();

    return result;
  }
}
