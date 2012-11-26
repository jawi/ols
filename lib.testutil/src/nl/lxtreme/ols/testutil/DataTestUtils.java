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
package nl.lxtreme.ols.testutil;


import java.io.*;
import java.net.*;
import java.util.regex.*;

import nl.lxtreme.ols.common.acquisition.*;


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

  /** The regular expression used to parse an (OLS-datafile) instruction. */
  private static final Pattern OLS_INSTRUCTION_PATTERN = Pattern.compile( "^;([^:]+):\\s+([^\r\n]+)$" );

  // METHODS

  /** The regular expression used to parse an (OLS-datafile) data value. */
  private static final Pattern OLS_DATA_PATTERN = Pattern.compile( "^([0-9a-fA-F]+)@(\\d+)$" );

  /**
   * Creates a new DataTestUtils instance.
   */
  private DataTestUtils()
  {
    // NO-op
  }

  /**
   * Creates a mocked data set with 16 sample/time values.
   * 
   * @param aChannelCount
   *          the number of <em>enabled</em> channels in the returned data
   *          container, > 0 && < 32.
   * @return a mocked data container, never <code>null</code>.
   */
  public static AcquisitionData generateAcquisitionData( final int aChannelCount )
  {
    return generateAcquisitionData( 16, aChannelCount );
  }

  /**
   * Creates a mocked data set with a given number of sample/time values.
   * 
   * @param aDataSize
   *          the number of sample/time values in the returned data container, >
   *          0;
   * @param aChannelCount
   *          the number of <em>enabled</em> channels in the returned data
   *          container, > 0 && < 32.
   * @return a mocked data container, never <code>null</code>.
   */
  public static AcquisitionData generateAcquisitionData( final int aDataSize, final int aChannelCount )
  {
    return generateAcquisitionData( aDataSize, aChannelCount, 1000000, -1 );
  }

  /**
   * Creates a mocked data set with a given number of sample/time values.
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
  public static AcquisitionData generateAcquisitionData( final int aDataSize, final int aChannelCount,
      final int aSampleRate, final int aTriggerPos )
  {
    return generateAcquisitionData( aDataSize, aChannelCount, aSampleRate, aTriggerPos, new DefaultTestDataProvider(
        aChannelCount ) );
  }

  /**
   * Creates a mocked data set with a given number of sample/time values.
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
  public static AcquisitionData generateAcquisitionData( final int aDataSize, final int aEnabledChannelCount,
      final int aSampleRate, final long aTriggerPos, final TestDataProvider aProvider )
  {
    final int[] values = new int[aDataSize];
    final long[] timestamps = new long[aDataSize];

    aProvider.fillData( values, timestamps, aDataSize );

    final int bitMask = ( int )( ( 1L << aEnabledChannelCount ) - 1L );

    return createAcquisitionResult( values, timestamps, aTriggerPos, aSampleRate, aEnabledChannelCount, bitMask,
        timestamps[aDataSize - 1] + 1L );
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
      return readCaptureData( new InputStreamReader( is ) );
    }
    finally
    {
      if ( is != null )
      {
        is.close();
      }
    }
  }

  /**
   * Returns a mocked captured data result.
   */
  public static AcquisitionData getMockedCapturedData()
  {
    return new AcquisitionData()
    {
      @Override
      public long getAbsoluteLength()
      {
        return 8L;
      }

      @Override
      public int getChannelCount()
      {
        return 8;
      }

      @Override
      public Channel[] getChannels()
      {
        return new Channel[0];
      }

      @Override
      public Cursor[] getCursors()
      {
        return new Cursor[0];
      }

      @Override
      public int getEnabledChannels()
      {
        return 0xFF;
      }

      @Override
      public int getSampleIndex( final long aTimeValue )
      {
        return ( int )aTimeValue;
      }

      @Override
      public int getSampleRate()
      {
        return 100;
      }

      @Override
      public long[] getTimestamps()
      {
        return new long[] { 1L, 2L, 3L, 4L };
      }

      @Override
      public long getTriggerPosition()
      {
        return -1L;
      }

      @Override
      public int[] getValues()
      {
        return new int[] { 1, 0, 1, 0 };
      }

      @Override
      public boolean hasTimingData()
      {
        return true;
      }

      @Override
      public boolean hasTriggerData()
      {
        return false;
      }

      @Override
      public boolean isCursorsVisible()
      {
        return true;
      }

      @Override
      public void setCursorsVisible( final boolean aVisible )
      {
        // Nop
      }
    };
  }

  /**
   * @param aValues
   * @param aTimestamps
   * @param aTriggerPos
   * @param aRate
   * @param aChannels
   * @param aEnabledChannels
   * @param aAbsoluteLength
   * @return
   */
  private static AcquisitionData createAcquisitionResult( final int[] aValues, final long[] aTimestamps,
      final long aTriggerPos, final int aRate, final int aChannels, final int aEnabledChannels,
      final long aAbsoluteLength )
  {
    return new AcquisitionData()
    {
      @Override
      public long getAbsoluteLength()
      {
        return aAbsoluteLength;
      }

      @Override
      public int getChannelCount()
      {
        return aChannels;
      }

      @Override
      public Channel[] getChannels()
      {
        return new Channel[0];
      }

      @Override
      public Cursor[] getCursors()
      {
        return new Cursor[0];
      }

      @Override
      public int getEnabledChannels()
      {
        return aEnabledChannels;
      }

      @Override
      public int getSampleIndex( final long aTimeValue )
      {
        return 0;
      }

      @Override
      public int getSampleRate()
      {
        return aRate;
      }

      @Override
      public long[] getTimestamps()
      {
        return aTimestamps;
      }

      @Override
      public long getTriggerPosition()
      {
        return aTriggerPos;
      }

      @Override
      public int[] getValues()
      {
        return aValues;
      }

      @Override
      public boolean hasTimingData()
      {
        return true;
      }

      @Override
      public boolean hasTriggerData()
      {
        return ( aTriggerPos >= 0 );
      }

      @Override
      public boolean isCursorsVisible()
      {
        return true;
      }

      @Override
      public void setCursorsVisible( final boolean aVisible )
      {
        // Nop
      }
    };
  }

  // CONSTANTS

  // METHODS

  /**
   * Reads the data from a given reader.
   * 
   * @param aProject
   *          the project to read the settings to;
   * @param aReader
   *          the reader to read the data from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  private static AcquisitionData readCaptureData( final Reader aReader ) throws IOException
  {
    // assume 'new' file format is in use, don't support uncompressed ones...
    boolean compressed = true;

    final AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

    final BufferedReader br = new BufferedReader( aReader );

    String line;
    while ( ( line = br.readLine() ) != null )
    {
      // Determine whether the line is an instruction, or data...
      final Matcher instructionMatcher = OLS_INSTRUCTION_PATTERN.matcher( line );
      final Matcher dataMatcher = OLS_DATA_PATTERN.matcher( line );

      if ( dataMatcher.matches() )
      {
        int value = Integer.parseInt( dataMatcher.group( 1 ), 16 );
        long timestamps = Long.parseLong( dataMatcher.group( 2 ) );

        builder.addSample( timestamps, value );
      }
      else if ( instructionMatcher.matches() )
      {
        // Ok; found an instruction...
        final String instrKey = instructionMatcher.group( 1 );
        final String instrValue = instructionMatcher.group( 2 );

        if ( "Size".equals( instrKey ) )
        {
          // Ignored..
        }
        else if ( "Rate".equals( instrKey ) )
        {
          builder.setSampleRate( Integer.parseInt( instrValue ) );
        }
        else if ( "Channels".equals( instrKey ) )
        {
          builder.setChannelCount( Integer.parseInt( instrValue ) );
        }
        else if ( "TriggerPosition".equals( instrKey ) )
        {
          long triggerPos = Long.parseLong( instrValue );
          if ( triggerPos >= 0 )
          {
            builder.setTriggerPosition( triggerPos );
          }
        }
        else if ( "EnabledChannels".equals( instrKey ) )
        {
          builder.setEnabledChannelMask( Integer.parseInt( instrValue ) );
        }
        else if ( "CursorEnabled".equals( instrKey ) )
        {
          // Ignored...
        }
        else if ( "Compressed".equals( instrKey ) )
        {
          compressed = Boolean.parseBoolean( instrValue );
        }
        else if ( "AbsoluteLength".equals( instrKey ) )
        {
          builder.setAbsoluteLength( Long.parseLong( instrValue ) );
        }
        else if ( instrKey.startsWith( "Cursor" ) )
        {
          // Ignored...
        }
      }
    }

    if ( !compressed )
    {
      throw new IOException( "Uncompressed data file found! Please send this file to the OLS developers!" );
    }

    return builder.build();
  }
}
