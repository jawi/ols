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
package nl.lxtreme.ols.tool;


import static org.junit.Assert.*;

import java.io.*;
import java.net.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;

import org.junit.*;


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
        if ( ( i % offset ) == 0 )
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
  public static void assertEquals( final AcquisitionData aExpected, final AcquisitionData aTested )
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
  public static void assertEquals( final String aMessage, final AcquisitionData aExpected,
      final AcquisitionData aTested )
  {
    assertNotNull( aExpected );
    assertNotNull( aTested );

    Assert.assertEquals( aMessage, aExpected.getAbsoluteLength(), aTested.getAbsoluteLength() );
    Assert.assertEquals( aMessage, aExpected.getChannelCount(), aTested.getChannelCount() );
    Assert.assertEquals( aMessage, aExpected.getEnabledChannels(), aTested.getEnabledChannels() );
    Assert.assertEquals( aMessage, aExpected.getSampleRate(), aTested.getSampleRate() );
    Assert.assertEquals( aMessage, aExpected.hasTimingData(), aTested.hasTimingData() );
    Assert.assertEquals( aMessage, aExpected.hasTriggerData(), aTested.hasTriggerData() );
    assertArrayEquals( aMessage, aExpected.getTimestamps(), aTested.getTimestamps() );
    assertArrayEquals( aMessage, aExpected.getValues(), aTested.getValues() );
  }

  /**
   * Creates a (mocked) tool context starting at the given sample index and
   * ending at the last available sample index.
   * 
   * @return a mocked tool context, never <code>null</code>.
   */
  public static ToolContext createToolContext( final AcquisitionData aContainer )
  {
    final int startSampleIdx = Math.max( 0, aContainer.getSampleIndex( aContainer.getTriggerPosition() ) - 1 );
    final int lastSampleIdx = aContainer.getValues().length - 1;
    return createToolContext( aContainer, startSampleIdx, lastSampleIdx );
  }

  /**
   * Creates a (mocked) tool context starting at the given sample index and
   * ending at the last available sample index.
   * 
   * @param aStartSampleIdx
   *          the starting sample index of the returned tool context;
   * @return a mocked tool context, never <code>null</code>.
   */
  public static ToolContext createToolContext( final AcquisitionData aContainer, final int aStartSampleIdx )
  {
    final int lastSampleIdx = aContainer.getValues().length - 1;
    return createToolContext( aContainer, aStartSampleIdx, lastSampleIdx );
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
  public static ToolContext createToolContext( final AcquisitionData aData, final int aStartSampleIdx,
      final int aLastSampleIdx )
  {
    final Integer first = Integer.valueOf( Math.max( 0, aStartSampleIdx ) );
    final Integer last = Integer.valueOf( Math.min( aLastSampleIdx, aData.getValues().length - 1 ) );
    final Integer size = Integer.valueOf( last.intValue() - first.intValue() );

    // Do NOT use Mockito#mock for this; it appears to slow things down *really*
    // much...
    return new ToolContext()
    {
      @Override
      public int getChannels()
      {
        return aData.getChannelCount();
      }

      @Override
      public Cursor getCursor( final int aSelectedIndex )
      {
        return null;
      }

      @Override
      public AcquisitionData getData()
      {
        return aData;
      }

      @Override
      public int getEnabledChannels()
      {
        return aData.getEnabledChannels();
      }

      @Override
      public int getEndSampleIndex()
      {
        return last.intValue();
      }

      @Override
      public int getLength()
      {
        return size.intValue();
      }

      @Override
      public int getStartSampleIndex()
      {
        return first.intValue();
      }
    };
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
  public static ToolContext createToolContext( final AcquisitionData aData, final long aStartTimestamp,
      final long aLastTimestamp )
  {
    int startIdx = aData.getSampleIndex( aStartTimestamp );
    int endIdx = aData.getSampleIndex( aLastTimestamp );
    return createToolContext( aData, startIdx, endIdx );
  }

  /**
   * Returns the given resource as project with captured data.
   * 
   * @param aResource
   *          the resource URL of the resource to get as datafile.
   * @return the data container with the given resource as captured data.
   */
  public static AcquisitionData getCapturedData( final URL aResource ) throws IOException
  {
    InputStream is = aResource.openStream();
    try
    {
      return OlsDataHelper.read( new InputStreamReader( is ) );
    }
    finally
    {
      try
      {
        if ( is != null )
        {
          is.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }
    }
  }
}
