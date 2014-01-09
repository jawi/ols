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
package nl.lxtreme.ols.tool.i2s;


import static org.junit.Assert.*;

import java.net.*;
import java.util.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.*;


/**
 * (Parameterized) tests cases for {@link I2SAnalyserTask}.
 */
@RunWith( Parameterized.class )
public class I2SAnalyserTaskDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int clockIdx;
  private final int dataIdx;
  private final int wsIdx;
  private final int expectedDatagramCount;
  private final int expectedBusErrorCount;

  // CONSTRUCTORS

  /**
   * Creates a new I2SAnalyserWorkerDataFilesTest instance.
   */
  public I2SAnalyserTaskDataFilesTest( final String aResourceName, final int aClockIdx, final int aDataIdx,
      final int aWsIdx, final int aExpectedDatagramCount, final int aExpectedBusErrorCount )
  {
    this.resourceName = aResourceName;
    this.clockIdx = aClockIdx;
    this.dataIdx = aDataIdx;
    this.wsIdx = aWsIdx;
    this.expectedDatagramCount = aExpectedDatagramCount;
    this.expectedBusErrorCount = aExpectedBusErrorCount;
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
        // { resource name, clockIdx, dataIdx, wsIdx, wordCount, errorCount }
        { "i2s-se16_44100.ols", 0, 2, 1, 11, 0 }, // 0
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.I2S.I2SAnalyserTask#doInBackground()}.
   */
  @Test
  public void testAnalyzeDataFile() throws Exception
  {
    I2SDataSet result = analyseDataFile( this.resourceName );

    assertBusErrorCount( result, this.expectedBusErrorCount );
    assertDataCount( result, this.expectedDatagramCount );
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
  private I2SDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener progressListener = Mockito.mock( ToolProgressListener.class );

    I2SAnalyserTask worker = new I2SAnalyserTask( toolContext, progressListener );
    worker.setDecodingArea( 0, container.getValues().length - 1 );
    worker.setClockIndex( this.clockIdx );
    worker.setDataIndex( this.dataIdx );
    worker.setWordSelectIndex( this.wsIdx );

    // Simulate we're running in a separate thread by directly calling the main
    // working routine...
    I2SDataSet result = worker.call();
    assertNotNull( result );

    return result;
  }

  private void assertBusErrorCount( final I2SDataSet aDataSet, final int aExpectedBusErrorCount )
  {
    int count = 0;
    for ( I2SData data : aDataSet.getData() )
    {
      if ( data.isEvent() && I2SDataSet.I2S_BUS_ERROR.equals( data.getEventName() ) )
      {
        count++;
      }
    }
    assertEquals( "Not all bus errors were seen?!", aExpectedBusErrorCount, count );
  }

  private void assertDataCount( final I2SDataSet aDataSet, final int aExpectedDataCount )
  {
    int count = 0;
    for ( I2SData data : aDataSet.getData() )
    {
      if ( !data.isEvent() )
      {
        count++;
      }
    }
    assertEquals( "Not all data datagrams were seen?!", aExpectedDataCount, count );
  }
}
