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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.dmx512;


import java.net.*;
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Test cases for {@link DMX512AnalyzerTask}.
 */
@SuppressWarnings( "boxing" )
public class DMX512AnalyzerDataFilesTest extends TestCase
{
  // METHODS

  /**
   * @return a collection of test data.
   */
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // { filename, error count, symbol count, dataline, slot count }
            { "DMX512_32channel_channel_1-32_val_1-32.ols", 0, 1123, 2, 32 }, //
            { "DMX512_512channel_all0.ols", 0, 2878, 2, 512 }, //
            { "DMX512_512channel_channel_1-32_val_1-32.ols", 0, 2633, 2, 512 }, //
        } );
  }

  /**
   * Test case for {@link DMX512AnalyzerTask#call()}.
   */
  public void testDMX512AnalysisOk() throws Exception
  {
    for ( Object[] params : getTestData() )
    {
      String resourceName = ( String )params[0];
      int errorCount = ( Integer )params[1];
      int symbolCount = ( Integer )params[2];
      int dataLine = ( Integer )params[3];
      Integer slotCount = ( Integer )params[4];

      AnnotationCollector result = analyseDataFile( resourceName, dataLine );

      assertEquals( errorCount, result.countDataErrors() );
      assertEquals( symbolCount, result.countSymbols() );

      DataAnnotation packetAnn = result.getFirstDataAnnotation( DMX512AnalyzerTask.EVENT_PACKET, dataLine );

      assertNotNull( packetAnn );
      assertEquals( slotCount, packetAnn.getProperties().get( DMX512AnalyzerTask.KEY_SLOT_COUNT ) );
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aChannelIdx ) throws Exception
  {
    AnnotationCollector annotationCollector = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, annotationCollector );

    Configuration config = new MapConfiguration();
    config.asMap().put( "channelIdx", Integer.valueOf( aChannelIdx ) );

    DMX512AnalyzerTask worker = new DMX512AnalyzerTask( toolContext, config );
    worker.call();

    return annotationCollector;
  }
}
