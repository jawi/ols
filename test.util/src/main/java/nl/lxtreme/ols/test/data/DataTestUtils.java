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
package nl.lxtreme.ols.test.data;


import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.net.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.test.data.project.*;
import nl.lxtreme.ols.util.*;

import org.junit.*;
import org.mockito.invocation.*;
import org.mockito.stubbing.*;


/**
 * Provides some convenience methods for handling (captured) data in tests.
 */
public final class DataTestUtils
{
  // INNER TYPES

  /**
   * Data provider for test data.
   */
  public static interface TestDataProvider
  {
    /**
     * @param aValues
     * @param aTimestamps
     * @param aDataSize
     */
    void fillData( final int[] aValues, final long[] aTimestamps, int aDataSize );
  }

  /**
   * Provides some default test data.
   */
  static class DefaultTestDataProvider implements TestDataProvider
  {
    private final int channelCount;

    /**
     * Creates a new DataTestUtils.DefaultTestDataProvider instance.
     */
    public DefaultTestDataProvider( final int aChannelCount )
    {
      this.channelCount = aChannelCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fillData( final int[] aValues, final long[] aTimestamps, final int aDataSize )
    {
      final int offset = ( this.channelCount < 2 ) ? 1 : ( this.channelCount / 2 );
      int value = 0xAA;
      for ( int i = 0; i < aDataSize; i++ )
      {
        if ( i % offset == 0 )
        {
          value = ( value == 0xAA ) ? 0x55 : 0xAA;
        }

        aValues[i] = value;
        aTimestamps[i] = ( i * 2 );
      }
    }
  }

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
   * Asserts that the given acquisition results are equal to each other.
   * 
   * @param aExpected
   *          the expected acquisition result, cannot be <code>null</code>;
   * @param aTested
   *          the acquisition result to test.
   */
  public static void assertEquals( final AcquisitionResult aExpected, final AcquisitionResult aTested )
  {
    assertEquals( "Captured data not equal!", aExpected, aTested );
  }

  /**
   * Asserts that the given acquisition results are equal to each other.
   * 
   * @param aMessage
   *          the message to display when the assertion fails;
   * @param aExpected
   *          the expected acquisition result, cannot be <code>null</code>;
   * @param aTested
   *          the acquisition result to test.
   */
  @SuppressWarnings( "boxing" )
  public static void assertEquals( final String aMessage, final AcquisitionResult aExpected,
      final AcquisitionResult aTested )
  {
    assertNotNull( aExpected );
    assertNotNull( aTested );

    Assert.assertEquals( aMessage, aExpected.getAbsoluteLength(), aTested.getAbsoluteLength() );
    Assert.assertEquals( aMessage, aExpected.getChannels(), aTested.getChannels() );
    Assert.assertEquals( aMessage, aExpected.getEnabledChannels(), aTested.getEnabledChannels() );
    Assert.assertEquals( aMessage, aExpected.getSampleRate(), aTested.getSampleRate() );
    Assert.assertEquals( aMessage, aExpected.hasTimingData(), aTested.hasTimingData() );
    Assert.assertEquals( aMessage, aExpected.hasTriggerData(), aTested.hasTriggerData() );
    assertArrayEquals( aMessage, aExpected.getTimestamps(), aTested.getTimestamps() );
    assertArrayEquals( aMessage, aExpected.getValues(), aTested.getValues() );
  }

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
    return createMockDataContainer( aDataSize, aChannelCount, 1000000 );
  }

  /**
   * Creates a mocked data container with a given number of sample/time values.
   * 
   * @param aDataSize
   *          the number of sample/time values in the returned data container, >
   *          0;
   * @param aChannelCount
   *          the number of <em>enabled</em> channels in the returned data
   *          container, > 0 && < 32;
   * @param aSampleRate
   *          the sample rate (in Hertz), > 0.
   * @return a mocked data container, never <code>null</code>.
   */
  public static DataContainer createMockDataContainer( final int aDataSize, final int aChannelCount,
      final int aSampleRate )
  {
    return createMockDataContainer( aDataSize, aChannelCount, aSampleRate, new DefaultTestDataProvider( aChannelCount ) );
  }

  /**
   * Creates a mocked data container with a given number of sample/time values.
   * 
   * @param aDataSize
   *          the number of sample/time values in the returned data container, >
   *          0;
   * @param aChannelCount
   *          the number of <em>enabled</em> channels in the returned data
   *          container, > 0 && < 32;
   * @param aSampleRate
   *          the sample rate (in Hertz), > 0;
   * @param aProvider
   *          the test data provider to use, cannot be <code>null</code>.
   * @return a mocked data container, never <code>null</code>.
   */
  public static DataContainer createMockDataContainer( final int aDataSize, final int aChannelCount,
      final int aSampleRate, final TestDataProvider aProvider )
  {
    final Project project = new StubTestProject();
    project.setChannelLabels( new String[32] );
    final ProjectManager pm = mock( ProjectManager.class );
    doReturn( project ).when( pm ).getCurrentProject();

    final int[] values = new int[aDataSize];
    final long[] timestamps = new long[aDataSize];

    aProvider.fillData( values, timestamps, aDataSize );

    final AcquisitionResult data = mock( AcquisitionResult.class );
    when( Integer.valueOf( data.getChannels() ) ).thenReturn( Integer.valueOf( Ols.MAX_CHANNELS ) );
    when( Long.valueOf( data.getAbsoluteLength() ) ).thenReturn( Long.valueOf( timestamps[aDataSize - 1] + 1L ) );
    when( Integer.valueOf( data.getEnabledChannels() ) ).thenReturn(
        Integer.valueOf( NumberUtils.getBitMask( aChannelCount ) ) );
    when( Integer.valueOf( data.getSampleRate() ) ).thenReturn( Integer.valueOf( aSampleRate ) );
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
    final int startSampleIdx = Math.max( 0, aContainer.getSampleIndex( aContainer.getTriggerPosition() ) - 1 );
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

  /**
   * Returns a mocked captured data result.
   */
  @SuppressWarnings( "boxing" )
  public static AcquisitionResult getMockedCapturedData()
  {
    AcquisitionResult result = mock( AcquisitionResult.class );
    when( result.getAbsoluteLength() ).thenReturn( Long.valueOf( 8 ) );
    when( result.getChannels() ).thenReturn( Integer.valueOf( 8 ) );
    when( result.getEnabledChannels() ).thenReturn( Integer.valueOf( 255 ) );
    when( result.getSampleIndex( anyLong() ) ).thenAnswer( new Answer<Integer>()
    {
      @Override
      public Integer answer( final InvocationOnMock aInvocation ) throws Throwable
      {
        final Long param = ( Long )aInvocation.getArguments()[0];
        return param.intValue();
      }
    } );
    when( result.getSampleRate() ).thenReturn( Integer.valueOf( 100 ) );
    when( result.getTimestamps() ).thenReturn( new long[] { 1L, 2L, 3L, 4L } );
    when( result.getValues() ).thenReturn( new int[] { 1, 0, 1, 0 } );
    when( result.hasTimingData() ).thenReturn( Boolean.TRUE );
    when( result.hasTriggerData() ).thenReturn( Boolean.FALSE );
    return result;
  }
}
