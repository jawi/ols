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
package nl.lxtreme.ols.tool.onewire;


import java.net.*;
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Creates a new {@link OneWireAnalyserWorkerTest}.
 */
@SuppressWarnings( "boxing" )
public class OneWireAnalyserWorkerTest extends TestCase
{
  // METHODS

  /**
   * @return a collection of test data.
   */
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // { resource name, bus-mode, data-line, datagram count, error count }
            { "ds18b20_1.ols", OneWireBusMode.STANDARD, 0, 40, 0, 13 }, // 0
            { "ow_minimal.ols", OneWireBusMode.STANDARD, 2, 48, 0, 1 }, // 1
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.onewire.OneWireAnalyserTask#doInBackground()}.
   */
  public void testAnalyzeDataFile() throws Exception
  {
    for ( Object[] params : getTestData() )
    {
      String resourceFile = ( String )params[0];
      OneWireBusMode busMode = ( OneWireBusMode )params[1];
      int dataLine = ( Integer )params[2];
      int datagramCount = ( Integer )params[3];
      int errorCount = ( Integer )params[4];
      int slavePresentPulseCount = ( Integer )params[5];

      final AnnotationCollector result = analyseDataFile( resourceFile, dataLine, busMode );

      assertEquals( errorCount, result.countDataErrors() );
      assertEquals( datagramCount, result.countSymbols() );
      assertEquals( slavePresentPulseCount, result.countDataAnnotations( OneWireAnalyserTask.EVENT_RESET,
          OneWireAnalyserTask.KEY_SLAVE_PRESENT, Boolean.TRUE ) );
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aChannelIdx,
      final OneWireBusMode aBusMode ) throws Exception
  {
    AnnotationCollector result = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, result );

    Configuration config = new MapConfiguration();
    config.asMap().put( "channelIdx", Integer.valueOf( aChannelIdx ) );
    config.asMap().put( "busMode", aBusMode );

    OneWireAnalyserTask worker = new OneWireAnalyserTask( toolContext, config );
    worker.call();

    return result;
  }
}
