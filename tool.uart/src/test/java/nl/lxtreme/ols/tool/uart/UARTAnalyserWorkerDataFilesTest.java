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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.uart;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * (Parameterized) tests cases for {@link UARTAnalyserWorker}.
 */
@RunWith( Parameterized.class )
public class UARTAnalyserWorkerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int expectedErrorCount;
  private final int expectedSymbolCount;
  private final int expectedBaudrate;
  private final int[] channels;

  // CONSTRUCTORS

  /**
   * Creates a new UARTAnalyserWorkerTest instance.
   */
  public UARTAnalyserWorkerDataFilesTest( final String aResourceName, final int aExpectedErrorCount,
      final int aExpectedSymbolCount, final int aExpectedBaudrate, final int... aChannels )
  {
    this.resourceName = aResourceName;
    this.expectedErrorCount = aExpectedErrorCount;
    this.expectedSymbolCount = aExpectedSymbolCount;
    this.expectedBaudrate = aExpectedBaudrate;
    this.channels = aChannels;
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
        // { filename, error count, symbol count, baudrate, (rxd, txd) }
            { "uart_8bit_1.ols", 0, 33, 57600, new int[] { 0, -1 } }, //
            { "uart_8bit_2.ols", 0, 6, 9600, new int[] { 2, -1 } }, //
        } );
  }

  /**
   * @param aContainer
   * @return
   */
  private static ToolContext createToolContext( final DataContainer aContainer )
  {
    final Integer first = Integer.valueOf( aContainer.getSampleIndex( aContainer.getTriggerPosition() ) - 1 );
    final Integer last = Integer.valueOf( aContainer.getValues().length - 1 );

    ToolContext toolContext = mock( ToolContext.class );
    when( toolContext.getStartSampleIndex() ).thenReturn( first );
    when( toolContext.getEndSampleIndex() ).thenReturn( last );
    when( toolContext.getLength() ).thenReturn( last );
    return toolContext;
  }

  /**
   * Returns the given resource as project with captured data.
   */
  private static DataContainer getCapturedData( final String aResourceName ) throws IOException
  {
    final URL resource = getResource( aResourceName );
    assertNotNull( resource );

    InputStream is = resource.openStream();
    try
    {
      final Project project = new ProjectImpl();
      project.setChannelLabels( new String[32] );
      OlsDataHelper.read( project, new InputStreamReader( is ) );

      ProjectManager projectMgr = mock( ProjectManager.class );
      when( projectMgr.getCurrentProject() ).thenReturn( project );

      return new DataContainer( projectMgr );
    }
    finally
    {
      HostUtils.closeResource( is );
    }
  }

  /**
   * Returns the resource with the given name from the datafiles directory.
   * 
   * @param aName
   *          the resource name, including file extension, cannot be
   *          <code>null</code>.
   * @return the URI pointing to the requested resource, never <code>null</code>
   *         .
   */
  private static URL getResource( final String aName )
  {
    final URL resource = UARTAnalyserWorkerDataFilesTest.class.getClassLoader().getResource( "datafiles/" + aName );
    if ( resource == null )
    {
      throw new RuntimeException( "Resource not found: " + aName );
    }
    return resource;
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.uart.UARTAnalyserWorker#doInBackground()}.
   */
  @Test
  public void testUartAnalysisOk() throws Exception
  {
    UARTDataSet result = analyseDataFile( this.resourceName );
    assertEquals( this.expectedErrorCount, result.getDetectedErrors() );
    assertEquals( this.expectedSymbolCount, result.getDecodedSymbols() );
    assertEquals( this.expectedBaudrate, result.getBaudRate() );
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
  private UARTDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    DataContainer container = getCapturedData( aResourceName );
    ToolContext toolContext = createToolContext( container );

    UARTAnalyserWorker worker = new UARTAnalyserWorker( container, toolContext );
    worker.setStopBits( UARTStopBits.STOP_1 );
    worker.setParity( UARTParity.NONE );
    worker.setBitCount( 8 );
    worker.setRxdIndex( this.channels[0] );
    worker.setTxdIndex( this.channels[1] );

    UARTDataSet result = worker.doInBackground();
    assertNotNull( result );
    return result;
  }
}
