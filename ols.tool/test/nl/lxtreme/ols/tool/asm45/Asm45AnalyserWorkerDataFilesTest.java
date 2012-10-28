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


import java.net.*;
import java.util.*;

import junit.framework.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.testutil.*;
import nl.lxtreme.ols.tool.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * (Parameterized) tests cases for {@link Asm45AnalyserTask}.
 * 
 * @author Ansgar Kueckes
 */
@SuppressWarnings( "boxing" )
public class Asm45AnalyserWorkerDataFilesTest extends TestCase
{
  // METHODS

  /**
   * @return a collection of test data.
   */
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
   * Test method for
   * {@link nl.lxtreme.ols.tool.asm45.Asm45AnalyserTask#doInBackground()}.
   */
  public void testAnalyzeDataFile() throws Exception
  {
    for ( Object[] params : getTestData() )
    {
      final String resourceName = ( String )params[0];
      final int lineSMCIdx = ( Integer )params[1];
      final int lineSTMIdx = ( Integer )params[2];
      final int lineEBGIdx = ( Integer )params[3];
      final int lineBYTEIdx = ( Integer )params[4];
      final int lineBLIdx = ( Integer )params[5];
      final int lineWRTIdx = ( Integer )params[6];
      final int lineSYNCIdx = ( Integer )params[7];
      final boolean decodeInstructions = ( Boolean )params[8];
      final boolean decodeDataTransfers = ( Boolean )params[9];
      final boolean decodeBusGrants = ( Boolean )params[10];
      final int expectedEventCount = ( Integer )params[11];

      AnnotationCollector result = analyseDataFile( resourceName, lineSMCIdx, lineSTMIdx, lineEBGIdx, lineBYTEIdx,
          lineBLIdx, lineWRTIdx, lineSYNCIdx, decodeInstructions, decodeDataTransfers, decodeBusGrants );

      assertEquals( expectedEventCount, result.countSymbols() );
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
  private AnnotationCollector analyseDataFile( final String aResourceName, final int aLineSMCIdx,
      final int aLineSTMIdx, final int aLineEBGIdx, final int aLineBYTEIdx, final int aLineBLIdx,
      final int aLineWRTIdx, final int aLineSYNCIdx, final boolean aDecodeInstructions,
      final boolean aDecodeDataTransfers, final boolean aDecodeBusGrants ) throws Exception
  {
    AnnotationCollector result = new AnnotationCollector();

    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionData container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = new TestToolContext( container, result );

    Configuration config = new MapConfiguration();
    config.asMap().put( "smcIdx", Integer.valueOf( aLineSMCIdx ) );
    config.asMap().put( "stmIdx", Integer.valueOf( aLineSTMIdx ) );
    config.asMap().put( "ebgIdx", Integer.valueOf( aLineEBGIdx ) );
    config.asMap().put( "byteIdx", Integer.valueOf( aLineBYTEIdx ) );
    config.asMap().put( "blIdx", Integer.valueOf( aLineBLIdx ) );
    config.asMap().put( "wrtIdx", Integer.valueOf( aLineWRTIdx ) );
    config.asMap().put( "syncIdx", Integer.valueOf( aLineSYNCIdx ) );
    config.asMap().put( "reportInst", Boolean.valueOf( aDecodeInstructions ) );
    config.asMap().put( "reportData", Boolean.valueOf( aDecodeDataTransfers ) );
    config.asMap().put( "reportBusGrants", Boolean.valueOf( aDecodeBusGrants ) );

    Asm45AnalyserTask worker = new Asm45AnalyserTask( toolContext, config );
    worker.call();

    return result;
  }
}
