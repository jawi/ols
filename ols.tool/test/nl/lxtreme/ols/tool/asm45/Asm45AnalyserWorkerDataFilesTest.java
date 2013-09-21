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
package nl.lxtreme.ols.tool.asm45;


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
 * (Parameterized) tests cases for {@link Asm45AnalyserTask}.
 * 
 * @author Ansgar Kueckes
 */
@RunWith( Parameterized.class )
public class Asm45AnalyserWorkerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int lineSMCidx;
  private final int lineSTMidx;
  private final int lineEBGidx;
  private final int lineBYTEidx;
  private final int lineBLidx;
  private final int lineWRTidx;
  private final int lineSYNCidx;

  private final boolean decodeInstructions;
  private final boolean decodeDataTransfers;
  private final boolean decodeBusGrants;

  private final int expectedEventCount;

  // CONSTRUCTORS

  /**
   * Creates a new Asm45AnalyserWorkerDataFilesTest instance.
   */
  public Asm45AnalyserWorkerDataFilesTest( final String aResourceName, final int aLineSMCIdx, final int aLineSTMIdx,
      final int aLineEBGIdx, final int aLineBYTEIdx, final int aLineBLIdx, final int aLineWRTIdx,
      final int aLineSYNCIdx, final boolean aDecodeInstructions, final boolean aDecodeDataTransfers,
      final boolean aDecodeBusGrants, final int aExpectedEventCount )
  {
    this.resourceName = aResourceName;
    this.lineSMCidx = aLineSMCIdx;
    this.lineSTMidx = aLineSTMIdx;
    this.lineEBGidx = aLineEBGIdx;
    this.lineBYTEidx = aLineBYTEIdx;
    this.lineBLidx = aLineBLIdx;
    this.lineWRTidx = aLineWRTIdx;
    this.lineSYNCidx = aLineSYNCIdx;
    this.decodeInstructions = aDecodeInstructions;
    this.decodeDataTransfers = aDecodeDataTransfers;
    this.decodeBusGrants = aDecodeBusGrants;
    this.expectedEventCount = aExpectedEventCount;
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
        // { resource name, SMC/, STM/, EBG, BYTE, BL, WRT/, SYNC,
        // decodeInstructions?, decodeDataTransfers?, decodeBusGrants?, expected
        // event count }
            { "asm45_test.ols", 22, 23, 25, 26, 27, 29, 30, true, false, false, 177 }, // 0
            { "asm45_test.ols", 22, 23, 25, 26, 27, 29, 30, true, true, false, 356 }, // 1
            { "asm45_test.ols", 22, 23, 25, 26, 27, 29, 30, true, true, true, 357 }, // 2
        } );
  }

  /**
   * @param aDataSet
   * @param aExpectedDataCount
   * @return
   */
  private static void assertDataCount( final Asm45DataSet aDataSet, final int aExpectedDataCount )
  {
    int count = aDataSet.getData().size();
    assertEquals( "Not all instructions/data transfers were seen?!", aExpectedDataCount, count );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.asm45.Asm45AnalyserTask#doInBackground()}.
   */
  @Test
  public void testAnalyzeDataFile() throws Exception
  {
    Asm45DataSet result = analyseDataFile( this.resourceName );
    assertDataCount( result, this.expectedEventCount );
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
  private Asm45DataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener toolProgressListener = Mockito.mock( ToolProgressListener.class );
    AnnotationListener annotationListener = Mockito.mock( AnnotationListener.class );

    Asm45AnalyserTask worker = new Asm45AnalyserTask( toolContext, toolProgressListener, annotationListener );
    worker.setLineSMCIndex( this.lineSMCidx );
    worker.setLineSTMIndex( this.lineSTMidx );
    worker.setLineEBGIndex( this.lineEBGidx );
    worker.setLineBYTEIndex( this.lineBYTEidx );
    worker.setLineBLIndex( this.lineBLidx );
    worker.setLineWRTIndex( this.lineWRTidx );
    worker.setLineSYNCIndex( this.lineSYNCidx );
    worker.setReportInst( this.decodeInstructions );
    worker.setReportData( this.decodeDataTransfers );
    worker.setReportBusGrants( this.decodeBusGrants );

    // Simulate we're running in a separate thread by directly calling the main
    // working routine...
    Asm45DataSet result = worker.call();
    assertNotNull( result );

    return result;
  }
}
