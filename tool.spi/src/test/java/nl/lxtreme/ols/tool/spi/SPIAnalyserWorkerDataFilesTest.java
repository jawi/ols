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
package nl.lxtreme.ols.tool.spi;


import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.NumberUtils.BitOrder;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * (Parameterized) tests cases for {@link SPIAnalyserWorker}.
 */
@RunWith( Parameterized.class )
public class SPIAnalyserWorkerDataFilesTest
{
  // VARIABLES

  private final String resourceName;
  private final int bitCount;
  private final int expectedMisoSymbolCount;
  private final int expectedMosiSymbolCount;
  private final SPIMode spiMode;
  private final BitOrder bitOrder;
  private final int[] channels;

  // CONSTRUCTORS

  /**
   * Creates a new SPIAnalyserWorkerDataFilesTest instance.
   */
  public SPIAnalyserWorkerDataFilesTest( final String aResourceName, final int aBitCount,
      final int aExpectedMisoSymbolCount, final int aExpectedMosiSymbolCount, final SPIMode aSPIMode,
      final BitOrder aBitOrder, final int... aChannels )
  {
    this.resourceName = aResourceName;
    this.bitCount = aBitCount;
    this.expectedMisoSymbolCount = aExpectedMisoSymbolCount;
    this.expectedMosiSymbolCount = aExpectedMosiSymbolCount;
    this.spiMode = aSPIMode;
    this.bitOrder = aBitOrder;
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
        // { filename, error count, MiSo symbol count, MoSi symbol count,
        // (MISO, MOSI, CS, SCLK) }
            { "spi_8bit_1.ols", 8, 7, 7, SPIMode.MODE_2, BitOrder.MSB_FIRST, new int[] { 0, 1, 3, 2 } }, //
            { "spi_8bit_2.ols", 8, 195, 195, SPIMode.MODE_2, BitOrder.MSB_FIRST, new int[] { 0, 1, 3, 2 } }, //
            { "spi_9bit_3.ols", 9, 15, 15, SPIMode.MODE_2, BitOrder.LSB_FIRST, new int[] { 0, 3, 1, 2 } }, //
        } );
  }

  /**
   * @param aContainer
   * @return
   */
  private static ToolContext createToolContext( final DataContainer aContainer )
  {
    final Integer first = Integer.valueOf( Math.max( 0,
        aContainer.getSampleIndex( aContainer.getTriggerPosition() ) - 1 ) );
    final Integer last = Integer.valueOf( aContainer.getValues().length - 1 );

    ToolContext toolContext = mock( ToolContext.class );
    when( toolContext.getStartSampleIndex() ).thenReturn( first );
    when( toolContext.getEndSampleIndex() ).thenReturn( last );
    when( toolContext.getLength() ).thenReturn( last - first );
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
    final URL resource = SPIAnalyserWorkerDataFilesTest.class.getClassLoader().getResource( "datafiles/" + aName );
    if ( resource == null )
    {
      throw new RuntimeException( "Resource not found: " + aName );
    }
    return resource;
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.tool.spi.SPIAnalyserWorker#doInBackground()}.
   */
  @Test
  public void testDoInBackground() throws Exception
  {
    SPIDataSet result = analyseDataFile( this.resourceName );
    assertEquals( this.expectedMisoSymbolCount, countEvents( result, SPIDataSet.SPI_MISO ) );
    assertEquals( this.expectedMosiSymbolCount, countEvents( result, SPIDataSet.SPI_MOSI ) );
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
  private SPIDataSet analyseDataFile( final String aResourceName ) throws Exception
  {
    DataContainer container = getCapturedData( aResourceName );
    ToolContext toolContext = createToolContext( container );

    SPIAnalyserWorker worker = new SPIAnalyserWorker( container, toolContext );
    worker.setBitCount( this.bitCount );
    worker.setHonourCS( true );
    worker.setReportCS( false );
    worker.setMode( this.spiMode );
    worker.setOrder( this.bitOrder );
    worker.setBitCount( this.bitCount );
    worker.setMisoIndex( this.channels[0] );
    worker.setMosiIndex( this.channels[1] );
    worker.setCSIndex( this.channels[2] );
    worker.setSCKIndex( this.channels[3] );

    SPIDataSet result = worker.doInBackground();
    assertNotNull( result );
    return result;
  }

  /**
   * @param aDataSet
   * @param aEventName
   * @return
   */
  private int countEvents( final SPIDataSet aDataSet, final String aEventName )
  {
    int count = 0;
    for ( SPIData data : aDataSet.getData() )
    {
      if ( aEventName.equals( data.getDataName() ) )
      {
        count++;
      }
    }
    return count;
  }

}
