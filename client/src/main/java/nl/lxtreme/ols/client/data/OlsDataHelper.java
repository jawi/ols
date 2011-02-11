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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.data;


import static nl.lxtreme.ols.util.NumberUtils.*;

import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.regex.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;
import nl.lxtreme.ols.util.*;


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
  /**
   * The time margin that is added to the last timestamp to obtain the absolute
   * length.
   */
  static final int ABS_TIME_MARGIN = 4;

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
  public static void read( final Project aProject, final Reader aReader ) throws IOException
  {
    int size = -1, rate = -1, channels = -1, enabledChannels = -1;
    long triggerPos = -1L;
    long absLen = -1L;

    boolean cursors = false;
    // assume 'new' file format is in use, don't support uncompressed ones...
    boolean compressed = true;

    Long[] cursorPositions = new Long[CapturedData.MAX_CURSORS];
    CapturedData capturedData = null;

    final BufferedReader br = new BufferedReader( aReader );
    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Parsing OLS captured data from stream..." );
    }

    try
    {
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
            size = safeParseInt( instrValue );
          }
          else if ( "Rate".equals( instrKey ) )
          {
            rate = safeParseInt( instrValue );
          }
          else if ( "Channels".equals( instrKey ) )
          {
            channels = safeParseInt( instrValue );
          }
          else if ( "TriggerPosition".equals( instrKey ) )
          {
            triggerPos = Long.parseLong( instrValue );
          }
          else if ( "EnabledChannels".equals( instrKey ) )
          {
            enabledChannels = safeParseInt( instrValue );
          }
          else if ( "CursorEnabled".equals( instrKey ) )
          {
            cursors = Boolean.parseBoolean( instrValue );
          }
          else if ( "Compressed".equals( instrKey ) )
          {
            compressed = Boolean.parseBoolean( instrValue );
          }
          else if ( "AbsoluteLength".equals( instrKey ) )
          {
            absLen = Long.parseLong( instrValue );
          }
          else if ( "CursorA".equals( instrKey ) )
          {
            final long value = safeParseLong( instrValue );
            cursorPositions[0] = ( value > Long.MIN_VALUE ) ? Long.valueOf( value ) : null;
          }
          else if ( "CursorB".equals( instrKey ) )
          {
            final long value = safeParseLong( instrValue );
            cursorPositions[1] = ( value > Long.MIN_VALUE ) ? Long.valueOf( value ) : null;
          }
          else if ( instrKey.startsWith( "Cursor" ) )
          {
            final int idx = safeParseInt( instrKey.substring( 6 ) );
            final long pos = Long.parseLong( instrValue );
            cursorPositions[idx] = ( pos > Long.MIN_VALUE ) ? Long.valueOf( pos ) : null;
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
      if ( rate <= 0 )
      {
        throw new IOException( "Data file is corrupt?! Sample rate is not provided!" );
      }
      if ( ( channels <= 0 ) || ( channels > 32 ) )
      {
        throw new IOException( "Data file is corrupt?! Channel count is not provided!" );
      }
      // Make sure the enabled channels are defined...
      if ( enabledChannels < 0 )
      {
        enabledChannels = NumberUtils.getBitMask( channels );
      }

      int[] values = new int[size];
      long[] timestamps = new long[size];

      try
      {
        for ( int i = 0; i < size; i++ )
        {
          final String[] dataPair = dataValues.get( i );

          values[i] = ( int )( Long.parseLong( dataPair[0], 16 ) & Integer.MAX_VALUE );
          timestamps[i] = Long.parseLong( dataPair[1], 10 ) & Long.MAX_VALUE;
        }
      }
      catch ( final NumberFormatException exception )
      {
        throw new IOException( "Invalid data encountered.", exception );
      }

      // Allow the absolute length to be undefined, in which case the last
      // time stamp is used (+ some margin to be able to see the last
      // sample)...
      long absoluteLength = Math.max( absLen, timestamps[size - 1] + ABS_TIME_MARGIN );

      // Finally set the captured data, and notify all event listeners...
      capturedData = new CapturedDataImpl( values, timestamps, triggerPos, rate, channels, enabledChannels,
          absoluteLength );
    }
    finally
    {
      aProject.setCapturedData( capturedData );
      aProject.setCursorPositions( cursorPositions );
      aProject.setCursorsEnabled( cursors );
    }
  }

  /**
   * Writes the data to the given writer.
   * 
   * @param aProject
   * @param aWriter
   *          the writer to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public static void write( final Project aProject, final Writer aWriter ) throws IOException
  {
    final BufferedWriter bw = new BufferedWriter( aWriter );

    final CapturedData capturedData = aProject.getCapturedData();

    final Long[] cursors = aProject.getCursorPositions();
    final boolean cursorsEnabled = aProject.isCursorsEnabled();

    try
    {
      final int[] values = capturedData.getValues();
      final long[] timestamps = capturedData.getTimestamps();

      bw.write( ";Size: " );
      bw.write( Integer.toString( values.length ) );
      bw.newLine();

      bw.write( ";Rate: " );
      bw.write( Integer.toString( capturedData.getSampleRate() ) );
      bw.newLine();

      bw.write( ";Channels: " );
      bw.write( Integer.toString( capturedData.getChannels() ) );
      bw.newLine();

      bw.write( ";EnabledChannels: " );
      bw.write( Integer.toString( capturedData.getEnabledChannels() ) );
      bw.newLine();

      if ( capturedData.hasTriggerData() )
      {
        bw.write( ";TriggerPosition: " );
        bw.write( Long.toString( capturedData.getTriggerPosition() ) );
        bw.newLine();
      }

      bw.write( ";Compressed: " );
      bw.write( Boolean.toString( true ) );
      bw.newLine();

      bw.write( ";AbsoluteLength: " );
      bw.write( Long.toString( capturedData.getAbsoluteLength() ) );
      bw.newLine();

      bw.write( ";CursorEnabled: " );
      bw.write( Boolean.toString( cursorsEnabled ) );
      bw.newLine();

      for ( int i = 0; cursorsEnabled && ( i < cursors.length ); i++ )
      {
        if ( cursors[i] != null )
        {
          bw.write( String.format( ";Cursor%d: ", Integer.valueOf( i ) ) );
          bw.write( cursors[i].toString() );
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
  static String formatSample( final int aValue, final long aTimestamp )
  {
    final String hexVal = Integer.toHexString( aValue & Integer.MAX_VALUE );
    final StringBuilder sb = new StringBuilder();
    sb.append( "00000000".substring( hexVal.length() ) );
    sb.append( hexVal );
    sb.append( "@" );
    sb.append( Long.toString( aTimestamp & Long.MAX_VALUE ) );
    return sb.toString();
  }
}
