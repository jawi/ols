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
package nl.lxtreme.ols.test.data;


import static org.mockito.Mockito.*;

import java.io.*;
import java.net.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.data.project.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides some convenience methods for handling (captured) data in tests.
 */
public final class DataTestUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new DataTestUtils instance.
   */
  private DataTestUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Creates a mocked data container with 16 sample/time values.
   * 
   * @param aChannelCount
   *          the number of <em>enabled</em> channels in the returned data
   *          container, > 0 && < 32.
   * @return a mocked data container, never <code>null</code>.
   */
  public static DataContainer createMockDataContainer( final int aChannelCount )
  {
    return createMockDataContainer( 16, aChannelCount );
  }

  /**
   * Creates a mocked data container with a given number of sample/time values.
   * 
   * @param aDataSize
   *          the number of sample/time values in the returned data container, >
   *          0;
   * @param aChannelCount
   *          the number of <em>enabled</em> channels in the returned data
   *          container, > 0 && < 32.
   * @return a mocked data container, never <code>null</code>.
   */
  public static DataContainer createMockDataContainer( final int aDataSize, final int aChannelCount )
  {
    final Project project = new StubTestProject();
    project.setChannelLabels( new String[32] );
    final ProjectManager pm = mock( ProjectManager.class );
    doReturn( project ).when( pm ).getCurrentProject();

    final int[] values = new int[aDataSize];
    final long[] timestamps = new long[aDataSize];
    final int offset = ( aChannelCount < 2 ) ? 1 : ( aChannelCount / 2 );
    int value = 0xAA;
    for ( int i = 0; i < aDataSize; i++ )
    {
      if ( i % offset == 0 )
      {
        value = ( value == 0xAA ) ? 0x55 : 0xAA;
      }

      values[i] = value;
      timestamps[i] = ( i * 2 );
    }

    final CapturedData data = mock( CapturedData.class );
    when( Integer.valueOf( data.getChannels() ) ).thenReturn( Integer.valueOf( CapturedData.MAX_CHANNELS ) );
    when( Long.valueOf( data.getAbsoluteLength() ) ).thenReturn( Long.valueOf( ( 2 * aDataSize ) + 1L ) );
    when( Integer.valueOf( data.getEnabledChannels() ) ).thenReturn(
        Integer.valueOf( NumberUtils.getBitMask( aChannelCount ) ) );
    when( Integer.valueOf( data.getSampleRate() ) ).thenReturn( Integer.valueOf( 1000000 ) );
    when( data.getValues() ).thenReturn( values );
    when( data.getTimestamps() ).thenReturn( timestamps );

    final DataContainer result = new DataContainer( pm );
    result.setCapturedData( data );
    return result;
  }

  /**
   * Creates a (mocked) tool context starting at the given sample index and
   * ending at the last available sample index.
   * 
   * @return a mocked tool context, never <code>null</code>.
   */
  public static ToolContext createToolContext( final DataContainer aContainer )
  {
    final int startSampleIdx = aContainer.getSampleIndex( aContainer.getTriggerPosition() ) - 1;
    final int lastSampleIdx = aContainer.getValues().length - 1;
    return createToolContext( startSampleIdx, lastSampleIdx );
  }

  /**
   * Creates a (mocked) tool context starting at the given sample index and
   * ending at the last available sample index.
   * 
   * @param aStartSampleIdx
   *          the starting sample index of the returned tool context;
   * @return a mocked tool context, never <code>null</code>.
   */
  public static ToolContext createToolContext( final DataContainer aContainer, final int aStartSampleIdx )
  {
    final int lastSampleIdx = aContainer.getValues().length - 1;
    return createToolContext( aStartSampleIdx, lastSampleIdx );
  }

  /**
   * Creates a (mocked) tool context starting and ending at the given sample
   * indexes.
   * 
   * @param aStartSampleIdx
   *          the starting sample index of the returned tool context;
   * @param aLastSampleIdx
   *          the ending sample index of the returned tool context.
   * @return a mocked tool context, never <code>null</code>.
   */
  public static ToolContext createToolContext( final int aStartSampleIdx, final int aLastSampleIdx )
  {
    final Integer size = Integer.valueOf( aLastSampleIdx - Math.max( 0, aStartSampleIdx ) );
    final Integer first = Integer.valueOf( Math.max( 0, aStartSampleIdx ) );
    final Integer last = Integer.valueOf( aLastSampleIdx );

    ToolContext toolContext = mock( ToolContext.class );
    when( Integer.valueOf( toolContext.getStartSampleIndex() ) ).thenReturn( first );
    when( Integer.valueOf( toolContext.getEndSampleIndex() ) ).thenReturn( last );
    when( Integer.valueOf( toolContext.getLength() ) ).thenReturn( size );
    return toolContext;
  }

  /**
   * Returns the given resource as project with captured data.
   * 
   * @param aResource
   *          the resource URL of the resource to get as datafile.
   * @return the data container with the given resource as captured data.
   */
  public static DataContainer getCapturedData( final URL aResource ) throws IOException
  {
    InputStream is = aResource.openStream();
    try
    {
      final Project project = new StubTestProject();
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
}
