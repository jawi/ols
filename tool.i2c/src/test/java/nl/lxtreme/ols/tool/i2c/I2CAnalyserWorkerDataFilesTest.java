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
 * (Parameterized) tests cases for {@link I2CAnalyserTask}.
 */
@RunWith( Parameterized.class )
public class I2CAnalyserWorkerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int lineAidx;
  private final int lineBidx;
  private final boolean autoDetectSDA;
  private final int expectedDatagramCount;
  private final int expectedBusErrorCount;

  protected int sclIdx;
  protected int sdaIdx;

  // CONSTRUCTORS

  /**
   * Creates a new I2CAnalyserWorkerDataFilesTest instance.
   */
  public I2CAnalyserWorkerDataFilesTest( final String aResourceName, final int aLineAIdx, final int aLineBIdx,
      final boolean aAutoDetectSDA, final int aExpectedDatagramCount, final int aExpectedBusErrorCount )
  {
    this.resourceName = aResourceName;
    this.lineAidx = aLineAIdx;
    this.lineBidx = aLineBIdx;
    this.autoDetectSDA = aAutoDetectSDA;
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
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private static void assertBusErrorCount( final I2CDataSet aDataSet, final int aExpectedBusErrorCount )
  {
    int count = 0;
    for ( I2CData data : aDataSet.getData() )
    {
      if ( data.isEvent() && I2CDataSet.I2C_BUS_ERROR.equals( data.getEventName() ) )
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
  private static void assertDataCount( final I2CDataSet aDataSet, final int aExpectedDataCount )
  {
    int count = 0;
    for ( I2CData data : aDataSet.getData() )
    {
      if ( !data.isEvent() )
      {
        count++;
      }
    }
    assertEquals( "Not all data datagrams were seen?!", aExpectedDataCount, count );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.i2c.I2CAnalyserTask#doInBackground()}.
   */
  @Test
  public void testAnalyzeDataFile() throws Exception
  {
    I2CDataSet result = analyseDataFile( this.resourceName );
    if ( this.autoDetectSDA )
    {
      assertEquals( "SCL not correctly detected?!", this.lineAidx, this.sclIdx );
      assertEquals( "SDA not correctly detected?!", this.lineBidx, this.sdaIdx );
    }
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
  private I2CDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener progressListener = Mockito.mock( ToolProgressListener.class );
    AnnotationListener annotationListener = Mockito.mock( AnnotationListener.class );

    I2CAnalyserTask worker = new I2CAnalyserTask( toolContext, progressListener, annotationListener );
    worker.setLineAIndex( this.lineAidx );
    worker.setLineBIndex( this.lineBidx );
    worker.setDetectSDA_SCL( this.autoDetectSDA );
    worker.setReportACK( false );
    worker.setReportNACK( false );
    worker.setReportStart( false );
    worker.setReportStop( false );

    // Simulate we're running in a separate thread by directly calling the main
    // working routine...
    I2CDataSet result = worker.call();
    assertNotNull( result );

    this.sclIdx = worker.getSclIdx();
    this.sdaIdx = worker.getSdaIdx();

    return result;
  }
}
