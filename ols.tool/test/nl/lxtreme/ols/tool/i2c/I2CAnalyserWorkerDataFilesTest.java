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
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * (Parameterized) tests cases for {@link I2CAnalyserTask}.
 */
public class I2CAnalyserWorkerDataFilesTest extends TestCase
{
  // VARIABLES

  protected int sclIdx;
  protected int sdaIdx;

  // METHODS

  /**
   * @return a collection of test data.
   */
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // { resource name, LineA/SCL, LineB/SDA, auto-detect SDA?, datagram
        // count, error count }
            { "i2c_1.ols", 0, 1, false, 5, 0 }, // 0
            { "i2c_2.ols", 1, 0, false, 239, 2 }, // 1
            { "i2c_2.ols", 1, 0, true, 239, 10 }, // 2
            { "i2c_5KHz.ols", 0, 1, false, 11, 0 }, // 3
            { "i2c_3.ols", 0, 1, false, 475, 1 }, // 4
            { "i2c_3.ols", 0, 1, true, 475, 0 }, // 5
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.i2c.I2CAnalyserTask#doInBackground()}.
   */
  @SuppressWarnings( "boxing" )
  public void testAnalyzeDataFile() throws Exception
  {
    for ( Object[] params : getTestData() )
    {
      String resource = ( String )params[0];
      int lineAidx = ( Integer )params[1];
      int lineBidx = ( Integer )params[2];
      boolean detectLines = ( Boolean )params[3];
      int symbols = ( Integer )params[4];
      int errors = ( Integer )params[5];

      AnnotationCollector result = analyseDataFile( resource, lineAidx, lineBidx, detectLines );
      if ( detectLines )
      {
        assertEquals( "SCL not correctly detected?!", lineAidx, this.sclIdx );
        assertEquals( "SDA not correctly detected?!", lineBidx, this.sdaIdx );
      }

      assertEquals( symbols, result.countSymbols() );
      assertEquals( errors, result.countDataErrors() );
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aLineAidx, final int aLineBidx,
      final boolean aDetectLines ) throws Exception
  {
    AnnotationCollector result = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, result );

    Configuration config = new MapConfiguration();
    config.asMap().put( "lineAIdx", Integer.valueOf( aLineAidx ) );
    config.asMap().put( "lineBIdx", Integer.valueOf( aLineBidx ) );
    config.asMap().put( "detectLines", Boolean.valueOf( aDetectLines ) );
    config.asMap().put( "reportAck", Boolean.FALSE );
    config.asMap().put( "reportNack", Boolean.FALSE );
    config.asMap().put( "reportStart", Boolean.FALSE );
    config.asMap().put( "reportStop", Boolean.FALSE );

    I2CAnalyserTask worker = new I2CAnalyserTask( toolContext, config );
    worker.call();

    this.sclIdx = worker.getSclIdx();
    this.sdaIdx = worker.getSdaIdx();

    return result;
  }
}
