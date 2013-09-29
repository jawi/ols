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
import java.util.*;
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
  @SuppressWarnings( "boxing" )
  public static AcquisitionData read( final Reader aReader ) throws IOException
  {
    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

    // assume 'new' file format is in use, don't support uncompressed ones...
    boolean compressed = true;
    int size = -1;

    Integer sampleRate = null;
    Integer channelCount = null;

    final BufferedReader br = new BufferedReader( aReader );
    final List<String[]> dataValues = new ArrayList<String[]>();

    String line;
    while ( ( line = br.readLine() ) != null )
    {
      // Determine whether the line is an instruction, or data...
      final Matcher instructionMatcher = OLS_INSTRUCTION_PATTERN.matcher( line );
      final Matcher dataMatcher = OLS_DATA_PATTERN.matcher( line );

      if ( dataMatcher.matches() )
      {
        final String[] dataPair = new String[] { dataMatcher.group( 1 ), dataMatcher.group( 2 ) };
        dataValues.add( dataPair );
      }
      else if ( instructionMatcher.matches() )
      {
        // Ok; found an instruction...
        final String instrKey = instructionMatcher.group( 1 );
        final String instrValue = instructionMatcher.group( 2 );

        if ( "Size".equals( instrKey ) )
        {
          size = Integer.parseInt( instrValue );
        }
        else if ( "Rate".equals( instrKey ) )
        {
          sampleRate = Integer.decode( instrValue );
          builder.setSampleRate( sampleRate.intValue() );
        }
        else if ( "Channels".equals( instrKey ) )
        {
          channelCount = Integer.decode( instrValue );
          builder.setChannelCount( channelCount.intValue() );
        }
        else if ( "TriggerPosition".equals( instrKey ) )
        {
          builder.setTriggerPosition( Long.parseLong( instrValue ) );
        }
        else if ( "EnabledChannels".equals( instrKey ) )
        {
          builder.setEnabledChannelMask( Integer.parseInt( instrValue ) );
        }
        else if ( "CursorEnabled".equals( instrKey ) )
        {
          builder.setCursorsVisible( Boolean.parseBoolean( instrValue ) );
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
            builder.setCursorTimestamp( idx, value );
          }
        }
      }
    }

    // Perform some sanity checks, make it not possible to import invalid
    // data...
    if ( dataValues.isEmpty() )
    {
      throw new IOException( "Data file does not contain any sample data!" );
    }
    if ( !compressed )
    {
      throw new IOException( "Uncompressed data file found! Please send this file to the OLS developers!" );
    }
    // In case the size is not provided (as of 0.9.4 no longer mandatory),
    // take the length of the data values as size indicator...
    if ( size < 0 )
    {
      size = dataValues.size();
    }
    if ( size != dataValues.size() )
    {
      throw new IOException( "Data file is corrupt?! Data size does not match sample count!" );
    }
    if ( sampleRate == null )
    {
      throw new IOException( "Data file is corrupt?! Sample rate is not provided!" );
    }
    if ( ( channelCount == null ) || ( channelCount <= 0 ) || ( channelCount > 32 ) )
    {
      throw new IOException( "Data file is corrupt?! Channel count is not provided!" );
    }

    try
    {
      for ( int i = 0; i < size; i++ )
      {
        final String[] dataPair = dataValues.get( i );

        long timestamp = Long.parseLong( dataPair[1], 10 ) & Long.MAX_VALUE;
        int value = ( int )Long.parseLong( dataPair[0], 16 );

        builder.addSample( timestamp, value );
      }
    }
    catch ( final NumberFormatException exception )
    {
      throw new IOException( "Invalid data encountered.", exception );
    }

    // Finally set the captured data, and notify all event listeners...
    return builder.build();
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
    final boolean cursorsEnabled = aData.isCursorsVisible();

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
