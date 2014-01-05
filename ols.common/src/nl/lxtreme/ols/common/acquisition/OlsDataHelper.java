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
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import java.io.*;
import java.util.regex.*;


/**
 * Helper class that is capable of reading & writing OLS data files.
 */
public final class OlsDataHelper
{
  // CONSTANTS

  /** The regular expression used to parse an (OLS-datafile) instruction. */
  private static final Pattern OLS_INSTRUCTION_PATTERN = Pattern.compile( "^;([^:]+):\\s+([^\r\n]+)$" );
  /** The regular expression used to parse an (OLS-datafile) data value. */
  private static final Pattern OLS_DATA_PATTERN = Pattern.compile( "^([0-9a-fA-F]+)@(\\d+)$" );

  // METHODS

  /**
   * Reads the text-based acquisition data from a given reader.
   * 
   * @param aReader
   *          the reader to read the data from, cannot be <code>null</code>.
   * @return the parsed acquisition data, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public static AcquisitionData read( Reader aReader ) throws IOException
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    try
    {
      read( aReader, builder );
    }
    finally
    {
      if ( aReader != null )
      {
        aReader.close();
      }
    }

    return builder.build();
  }

  /**
   * Reads the text-based acquisition data from a given reader.
   * 
   * @param aReader
   *          the reader to read the data from, cannot be <code>null</code>;
   * @param aBuilder
   *          the {@link AcquisitionDataBuilder} to use to build the
   *          {@link AcquisitionData}, cannot be <code>null</code>.
   * @return the parsed acquisition data, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems or invalid/inconsistent acquisition data.
   */
  public static void read( Reader aReader, AcquisitionDataBuilder aBuilder ) throws IOException
  {
    Integer sampleRate = null;
    Integer channelCount = null;
    BufferedReader br = new BufferedReader( aReader );

    int expectedSamples = -1;
    int readSamples = 0;

    try
    {
      String line;
      while ( ( line = br.readLine() ) != null )
      {
        // Determine whether the line is an instruction, or data...
        Matcher instructionMatcher = OLS_INSTRUCTION_PATTERN.matcher( line );
        Matcher dataMatcher = OLS_DATA_PATTERN.matcher( line );

        if ( dataMatcher.matches() )
        {
          int value = ( int )Long.parseLong( dataMatcher.group( 1 ), 16 );
          long timestamp = Long.parseLong( dataMatcher.group( 2 ), 10 ) & Long.MAX_VALUE;

          aBuilder.addSample( timestamp, value );
          readSamples++;
        }
        else if ( instructionMatcher.matches() )
        {
          // Ok; found an instruction...
          final String instrKey = instructionMatcher.group( 1 );
          final String instrValue = instructionMatcher.group( 2 );

          if ( "Size".equals( instrKey ) )
          {
            // No longer used/needed as of 0.9.4...
            expectedSamples = Integer.parseInt( instrValue );
          }
          else if ( "Rate".equals( instrKey ) )
          {
            sampleRate = Integer.decode( instrValue );
            aBuilder.setSampleRate( sampleRate.intValue() );
          }
          else if ( "Channels".equals( instrKey ) )
          {
            channelCount = Integer.decode( instrValue );
            aBuilder.setChannelCount( channelCount.intValue() );
          }
          else if ( "TriggerPosition".equals( instrKey ) )
          {
            aBuilder.setTriggerPosition( Long.parseLong( instrValue ) );
          }
          else if ( "EnabledChannels".equals( instrKey ) )
          {
            aBuilder.setEnabledChannelMask( Integer.parseInt( instrValue ) );
          }
          else if ( "CursorEnabled".equals( instrKey ) )
          {
            aBuilder.setCursorsVisible( Boolean.parseBoolean( instrValue ) );
          }
          else if ( "Compressed".equals( instrKey ) )
          {
            if ( !Boolean.parseBoolean( instrValue ) )
            {
              throw new IOException( "Uncompressed data file found! Please send this file to the OLS developers!" );
            }
          }
          else if ( "AbsoluteLength".equals( instrKey ) )
          {
            aBuilder.setAbsoluteLength( Long.parseLong( instrValue ) );
          }
          else if ( instrKey.startsWith( "Cursor" ) )
          {
            String idxValue = instrKey.substring( 6 );
            final long value = Long.parseLong( instrValue );

            int idx;
            if ( "A".equalsIgnoreCase( idxValue ) )
            {
              idx = 0;
            }
            else if ( "B".equalsIgnoreCase( idxValue ) )
            {
              idx = 1;
            }
            else
            {
              idx = Integer.parseInt( idxValue );
            }

            if ( value > Long.MIN_VALUE )
            {
              aBuilder.add( aBuilder.createCursor().setIndex( idx ).setTimestamp( value ) );
            }
          }
        }
      }
    }
    catch ( NumberFormatException exception )
    {
      throw new IOException( "Invalid data encountered!", exception );
    }

    if ( sampleRate == null )
    {
      throw new IOException( "Data file is corrupt?! Sample rate is not provided!" );
    }
    if ( channelCount == null )
    {
      throw new IOException( "Data file is corrupt?! Channel count is not provided!" );
    }
    if ( readSamples == 0 )
    {
      throw new IOException( "Data file is corrupt?! No data found!" );
    }
    if ( expectedSamples >= 0 && ( expectedSamples != readSamples ) )
    {
      throw new IOException( "Data file is corrupt?! Data size does not match sample count!" );
    }
  }

  /**
   * Writes the given acquisition data in a text-based format to the given
   * writer.
   * 
   * @param aWriter
   *          the writer to write the data to, cannot be <code>null</code>;
   * @param aData
   *          the acquisition data to write, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public static void write( final Writer aWriter, final AcquisitionData aData ) throws IOException
  {
    final BufferedWriter bw = new BufferedWriter( aWriter );

    final Cursor[] cursors = aData.getCursors();
    final boolean cursorsEnabled = aData.areCursorsVisible();

    try
    {
      final int[] values = aData.getValues();
      final long[] timestamps = aData.getTimestamps();

      bw.write( ";Size: " );
      bw.write( Integer.toString( values.length ) );
      bw.newLine();

      bw.write( ";Rate: " );
      bw.write( Integer.toString( aData.getSampleRate() ) );
      bw.newLine();

      bw.write( ";Channels: " );
      bw.write( Integer.toString( aData.getChannelCount() ) );
      bw.newLine();

      bw.write( ";EnabledChannels: " );
      bw.write( Integer.toString( aData.getEnabledChannels() ) );
      bw.newLine();

      if ( aData.hasTriggerData() )
      {
        bw.write( ";TriggerPosition: " );
        bw.write( Long.toString( aData.getTriggerPosition() ) );
        bw.newLine();
      }

      bw.write( ";Compressed: " );
      bw.write( Boolean.toString( true ) );
      bw.newLine();

      bw.write( ";AbsoluteLength: " );
      bw.write( Long.toString( aData.getAbsoluteLength() ) );
      bw.newLine();

      bw.write( ";CursorEnabled: " );
      bw.write( Boolean.toString( cursorsEnabled ) );
      bw.newLine();

      for ( int i = 0; cursorsEnabled && ( i < cursors.length ); i++ )
      {
        if ( cursors[i].isDefined() )
        {
          bw.write( String.format( ";Cursor%d: ", Integer.valueOf( i ) ) );
          bw.write( Long.toString( cursors[i].getTimestamp() ) );
          bw.newLine();
        }
      }
      for ( int i = 0; i < values.length; i++ )
      {
        bw.write( formatSample( values[i], timestamps[i] ) );
        bw.newLine();
      }
    }
    finally
    {
      bw.flush();
    }
  }

  /**
   * Formats the given value and timestamp into a single sample string.
   * 
   * @param aValue
   *          the sample value to format;
   * @param aTimestamp
   *          the timestamp to format.
   * @return the sample string, in the form of
   *         &lt;value<sub>16</sub>&gt;@&lt;timestamp<sub>10</sub>&gt;.
   */
  @SuppressWarnings( "boxing" )
  static String formatSample( final int aValue, final long aTimestamp )
  {
    // values can become negative (full 32-bit is used!), while timestamps never
    // can be negative (it is a relative timestamp!)...
    return String.format( "%08x@%d", aValue, ( aTimestamp & Long.MAX_VALUE ) );
  }
}
