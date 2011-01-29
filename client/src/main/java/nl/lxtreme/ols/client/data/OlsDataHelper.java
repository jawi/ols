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


import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.regex.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.project.*;


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
  public static void read( final Project aProject, final Reader aReader ) throws IOException
  {
    int size = -1, rate = -1, channels = 32, enabledChannels = -1;
    long triggerPos = -1;

    Long[] cursorPositions = new Long[CapturedData.MAX_CURSORS];
    CapturedData capturedData = null;

    boolean cursors = false;
    boolean compressed = false;
    long absLen = 0;

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
            size = Integer.parseInt( instrValue );
          }
          else if ( "Rate".equals( instrKey ) )
          {
            rate = Integer.parseInt( instrValue );
          }
          else if ( "Channels".equals( instrKey ) )
          {
            channels = Integer.parseInt( instrValue );
          }
          else if ( "TriggerPosition".equals( instrKey ) )
          {
            triggerPos = Long.parseLong( instrValue );
          }
          else if ( "EnabledChannels".equals( instrKey ) )
          {
            enabledChannels = Integer.parseInt( instrValue );
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
            final long value = Long.parseLong( instrValue );
            cursorPositions[0] = ( value > Long.MIN_VALUE ) ? Long.valueOf( value ) : null;
          }
          else if ( "CursorB".equals( instrKey ) )
          {
            final long value = Long.parseLong( instrValue );
            cursorPositions[1] = ( value > Long.MIN_VALUE ) ? Long.valueOf( value ) : null;
          }
          else if ( instrKey.startsWith( "Cursor" ) )
          {
            final int idx = Integer.parseInt( instrKey.substring( 6 ) );
            final long pos = Long.parseLong( instrValue );
            cursorPositions[idx] = ( pos > Long.MIN_VALUE ) ? Long.valueOf( pos ) : null;
          }
        }
      }

      long absoluteLength;
      int[] values;
      long[] timestamps;

      if ( dataValues.isEmpty() || ( size < 0 ) )
      {
        throw new IOException( "File does not appear to be a valid datafile!" );
      }

      if ( !compressed )
      {
        throw new IOException(
            "Uncompressed data file found! Please sent this file to the OLS developers for further inspection!" );
      }
      else if ( size != dataValues.size() )
      {
        throw new IOException( "Data size mismatch! Corrupt file encountered!" );
      }
      else
      {
        // new compressed file format
        absoluteLength = absLen;
        values = new int[size];
        timestamps = new long[size];

        try
        {
          for ( int i = 0; i < dataValues.size(); i++ )
          {
            final String[] dataPair = dataValues.get( i );

            values[i] = Integer.parseInt( dataPair[0].substring( 0, 4 ), 16 ) << 16
                | Integer.parseInt( dataPair[0].substring( 4, 8 ), 16 );

            timestamps[i] = Long.parseLong( dataPair[1] );
          }
        }
        catch ( final NumberFormatException exception )
        {
          throw new IOException( "Invalid data encountered." );
        }
      }

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
    final boolean cursorsEnabled = ( cursors != null ) && ( cursors.length > 0 );

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
        final String hexVal = Integer.toHexString( values[i] );
        bw.write( "00000000".substring( hexVal.length() ) );
        bw.write( hexVal );
        bw.write( "@" );
        bw.write( Long.toString( timestamps[i] ) );
        bw.newLine();
      }
    }
    finally
    {
      bw.flush();
    }
  }
}
