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


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.*;
import nl.lxtreme.ols.test.data.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.*;


/**
 * Creates a new {@link OneWireAnalyserWorkerTest}.
 */
@RunWith( Parameterized.class )
public class OneWireAnalyserWorkerTest
{
  // VARIABLES

  private final String resourceFile;
  private final OneWireBusMode busMode;
  private final int channelIdx;
  private final int datagramCount;
  private final int errorCount;
  private final int slavePresentPulseCount;

  // CONSTRUCTORS

  /**
   * Creates a new OneWireAnalyserWorkerTest instance.
   */
  public OneWireAnalyserWorkerTest( final String aResourceFile, final OneWireBusMode aBusMode, final int aChannelIdx,
      final int aDatagramCount, final int aErrorCount, final int aSlavePresentPulseCount )
  {
    this.resourceFile = aResourceFile;
    this.busMode = aBusMode;
    this.channelIdx = aChannelIdx;
    this.datagramCount = aDatagramCount;
    this.errorCount = aErrorCount;
    this.slavePresentPulseCount = aSlavePresentPulseCount;
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
        // { resource name, bus-mode, data-line, datagram count, error count }
            { "ds18b20_1.ols", OneWireBusMode.STANDARD, 0, 40, 0, 13 }, // 0
            { "ow_minimal.ols", OneWireBusMode.STANDARD, 2, 48, 0, 1 }, // 1
        } );
  }

  /**
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private static void assertBusErrorCount( final OneWireDataSet aDataSet, final int aExpectedBusErrorCount )
  {
    int count = 0;
    for ( OneWireData data : aDataSet.getData() )
    {
      if ( data.isEvent() && OneWireDataSet.OW_BUS_ERROR.equals( data.getEventName() ) )
      {
        count++;
      }
    }
    assertEquals( "Not all bus errors were seen?!", aExpectedBusErrorCount, count );
  }

  /**
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private static void assertDatagramCount( final OneWireDataSet aDataSet, final int aExpectedDataCount )
  {
    int count = 0;
    for ( OneWireData data : aDataSet.getData() )
    {
      if ( !data.isEvent() )
      {
        count++;
      }
    }
    assertEquals( "Not all data datagrams were seen?!", aExpectedDataCount, count );
  }

  /**
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private static void assertSlavePresentPulseCount( final OneWireDataSet aDataSet, final int aExpectedCount )
  {
    int count = 0;
    for ( OneWireData data : aDataSet.getData() )
    {
      if ( data.isEvent() && ( data.getValue() == 1 ) )
      {
        count++;
      }
    }
    assertEquals( "Not all data datagrams were seen?!", aExpectedCount, count );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.onewire.OneWireAnalyserTask#doInBackground()}.
   */
  @Test
  public void testAnalyzeDataFile() throws Exception
  {
    final OneWireDataSet dataSet = analyseDataFile( this.resourceFile );

    assertBusErrorCount( dataSet, this.errorCount );
    assertDatagramCount( dataSet, this.datagramCount );
    assertSlavePresentPulseCount( dataSet, this.slavePresentPulseCount );
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
  private OneWireDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener toolProgressListener = Mockito.mock( ToolProgressListener.class );
    AnnotationListener annotationListener = Mockito.mock( AnnotationListener.class );

    OneWireAnalyserTask worker = new OneWireAnalyserTask( toolContext, toolProgressListener, annotationListener );
    worker.setOneWireLineIndex( this.channelIdx );
    worker.setOneWireBusMode( this.busMode );

    // Simulate we're running in a separate thread by directly calling the main
    // working routine...
    OneWireDataSet result = worker.call();
    assertNotNull( result );

    return result;
  }
}
