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
package nl.lxtreme.ols.client.project.impl;


import java.io.*;
import java.util.logging.*;
import java.util.regex.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Helper class that is capable of reading & writing OLS data files.
 */
public final class OlsDataHelper
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( OlsDataHelper.class.getName() );

  /** The regular expression used to parse an (OLS-datafile) instruction. */
  private static final Pattern OLS_INSTRUCTION_PATTERN = Pattern.compile( "^;([^:]+):\\s+([^\r\n]+)$" );
  /** The regular expression used to parse an (OLS-datafile) data value. */
  private static final Pattern OLS_DATA_PATTERN = Pattern.compile( "^([0-9a-fA-F]+)@(\\d+)$" );

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
  public static void read( final Session aSession, final Reader aReader ) throws IOException
  {
    // assume 'new' file format is in use, don't support uncompressed ones...
    boolean compressed = true;

    final AcquisitionDataBuilder builder = new AcquisitionDataBuilder();

    final BufferedReader br = new BufferedReader( aReader );
    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Parsing OLS captured data from stream..." );
    }

    String line;
    while ( ( line = br.readLine() ) != null )
    {
      // Determine whether the line is an instruction, or data...
      final Matcher instructionMatcher = OLS_INSTRUCTION_PATTERN.matcher( line );
      final Matcher dataMatcher = OLS_DATA_PATTERN.matcher( line );

      if ( dataMatcher.matches() )
      {
        int sampleValue = Integer.parseInt( dataMatcher.group( 1 ), 16 );
        long timestamp = Long.parseLong( dataMatcher.group( 2 ) );

        builder.addSample( timestamp, sampleValue );
      }
      else if ( instructionMatcher.matches() )
      {
        // Ok; found an instruction...
        final String instrKey = instructionMatcher.group( 1 );
        final String instrValue = instructionMatcher.group( 2 );

        if ( "Size".equals( instrKey ) )
        {
          // Ignored; no longer needed...
        }
        else if ( "Rate".equals( instrKey ) )
        {
          builder.setSampleRate( safeParseInt( instrValue ) );
        }
        else if ( "Channels".equals( instrKey ) )
        {
          builder.setChannelCount( safeParseInt( instrValue ) );
        }
        else if ( "TriggerPosition".equals( instrKey ) )
        {
          builder.setTriggerPosition( safeParseLong( instrValue ) );
        }
        else if ( "EnabledChannels".equals( instrKey ) )
        {
          builder.setEnabledChannelMask( safeParseInt( instrValue ) );
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
          builder.setAbsoluteLength( safeParseLong( instrValue ) );
        }
        else if ( "CursorA".equals( instrKey ) )
        {
          final long value = safeParseLong( instrValue );
          if ( value > Long.MIN_VALUE )
          {
            builder.setCursorTimestamp( 0, value );
          }
        }
        else if ( "CursorB".equals( instrKey ) )
        {
          final long value = safeParseLong( instrValue );
          if ( value > Long.MIN_VALUE )
          {
            builder.setCursorTimestamp( 1, value );
          }
        }
        else if ( instrKey.startsWith( "Cursor" ) )
        {
          final int idx = safeParseInt( instrKey.substring( 6 ) );
          final long pos = Long.parseLong( instrValue );
          if ( pos > Long.MIN_VALUE )
          {
            builder.setCursorTimestamp( idx, pos );
          }
        }
      }
    }

    if ( !compressed )
    {
      throw new IOException( "Uncompressed data file found! Please send this file to the OLS developers!" );
    }

    // Publish the acquisition data to our session...
    aSession.setAcquisitionData( builder.build() );
  }

  /**
   * Writes the data to the given writer.
   * 
   * @param aSession
   *          the session data to write, cannot be <code>null</code>;
   * @param aWriter
   *          the writer to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public static void write( final Session aSession, final Writer aWriter ) throws IOException
  {
    final BufferedWriter bw = new BufferedWriter( aWriter );

    final AcquisitionData data = aSession.getAcquisitionData();

    try
    {
      final int[] values = data.getValues();
      final long[] timestamps = data.getTimestamps();

      bw.write( ";Size: " );
      bw.write( Integer.toString( values.length ) );
      bw.newLine();

      bw.write( ";Rate: " );
      bw.write( Integer.toString( data.getSampleRate() ) );
      bw.newLine();

      bw.write( ";Channels: " );
      bw.write( Integer.toString( data.getChannelCount() ) );
      bw.newLine();

      bw.write( ";EnabledChannels: " );
      bw.write( Integer.toString( data.getEnabledChannels() ) );
      bw.newLine();

      if ( data.hasTriggerData() )
      {
        bw.write( ";TriggerPosition: " );
        bw.write( Long.toString( data.getTriggerPosition() ) );
        bw.newLine();
      }

      bw.write( ";Compressed: " );
      bw.write( Boolean.toString( true ) );
      bw.newLine();

      bw.write( ";AbsoluteLength: " );
      bw.write( Long.toString( data.getAbsoluteLength() ) );
      bw.newLine();

      bw.write( ";CursorEnabled: " );
      bw.write( Boolean.toString( data.isCursorsVisible() ) );
      bw.newLine();

      for ( Cursor cursor : data.getCursors() )
      {
        if ( cursor.isDefined() )
        {
          final Integer idx = Integer.valueOf( cursor.getIndex() );
          bw.write( String.format( ";Cursor%d: ", idx ) );
          bw.write( Long.toString( cursor.getTimestamp() ) );
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

  private static int safeParseInt( final String aText )
  {
    try
    {
      return Integer.parseInt( aText );
    }
    catch ( NumberFormatException exception )
    {
      return -1;
    }
  }

  private static long safeParseLong( final String aText )
  {
    try
    {
      return Long.parseLong( aText );
    }
    catch ( NumberFormatException exception )
    {
      return -1L;
    }
  }
}
