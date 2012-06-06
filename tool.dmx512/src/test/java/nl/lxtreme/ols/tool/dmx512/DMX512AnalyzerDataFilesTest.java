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
import org.junit.runners.Parameterized.*;
import org.mockito.*;


/**
 * Test cases for {@link DMX512AnalyzerTask}.
 */
@RunWith( Parameterized.class )
public class DMX512AnalyzerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int expectedErrorCount;
  private final int expectedSymbolCount;
  private final int channelIdx;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DMX512AnalyzerDataFilesTest} instance.
   */
  public DMX512AnalyzerDataFilesTest( final String aResourceName, final int aExpectedErrorCount,
      final int aExpectedSymbolCount, final int aChannelIdx )
  {
    this.resourceName = aResourceName;
    this.expectedErrorCount = aExpectedErrorCount;
    this.expectedSymbolCount = aExpectedSymbolCount;
    this.channelIdx = aChannelIdx;
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
        // { filename, error count, symbol count, dataline }
            { "DMX512_32channel_channel_1-32_val_1-32.ols", 0, 1123, 2 }, //
            { "DMX512_512channel_all0.ols", 0, 2878, 2 }, //
            { "DMX512_512channel_channel_1-32_val_1-32.ols", 0, 2633, 2 }, //
        } );
  }

  /**
   * Test case for {@link DMX512AnalyzerTask#call()}.
   */
  @Test
  public void testDMX512AnalysisOk() throws Exception
  {
    DMX512DataSet result = analyseDataFile( this.resourceName );

    assertEquals( this.expectedErrorCount, result.getDetectedErrors() );
    assertEquals( this.expectedSymbolCount, result.getDecodedSymbols() );
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
  private DMX512DataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    URL resource = ResourceUtils.getResource( getClass(), aResourceName );
    AcquisitionResult container = DataTestUtils.getCapturedData( resource );
    ToolContext toolContext = DataTestUtils.createToolContext( container );

    ToolProgressListener tpl = Mockito.mock( ToolProgressListener.class );
    AnnotationListener al = Mockito.mock( AnnotationListener.class );

    DMX512AnalyzerTask worker = new DMX512AnalyzerTask( toolContext, tpl, al );
    worker.setDataLine( this.channelIdx );

    DMX512DataSet result = worker.call();
    assertNotNull( result );
    return result;
  }
}
