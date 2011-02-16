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

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;


/**
 * @author jawi
 */
public class UARTAnalyserWorkerTest
{
  // METHODS

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
      project.setChannelLabels( new String[] { "RxD", "TxD" } );
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
    final URL resource = UARTAnalyserWorkerTest.class.getClassLoader().getResource( "datafiles/" + aName );
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
  public void test8bitUartAnalysis_Sample1Ok() throws Exception
  {
    DataContainer container = getCapturedData( "uart_8bit_1.ols" );
    ToolContext toolContext = createToolContext( container );

    UARTAnalyserWorker worker = new UARTAnalyserWorker( container, toolContext );
    worker.setStopBits( UARTStopBits.STOP_1 );
    worker.setParity( UARTParity.NONE );
    worker.setBitCount( 8 );
    worker.setRxdIndex( 0 );
    worker.setTxdIndex( 1 );

    UARTDataSet result = worker.doInBackground();
    assertNotNull( result );
  }
}
