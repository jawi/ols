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


import static nl.lxtreme.ols.util.NumberUtils.*;

import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.util.regex.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


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
  @SuppressWarnings( "boxing" )
  public static DataSetImpl read( final Reader aReader ) throws IOException
  {
    int size = -1;
    Integer rate = null, channels = null, enabledChannels = null;
    long triggerPos = -1L;
    long absLen = -1L;

    // assume 'new' file format is in use, don't support uncompressed ones...
    boolean compressed = true;

    AcquisitionResult capturedData = null;
    DataSetImpl tempDataSet = new DataSetImpl();

    final BufferedReader br = new BufferedReader( aReader );
    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Parsing OLS captured data from stream..." );
    }

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
          tempDataSet.setCursorsEnabled( Boolean.parseBoolean( instrValue ) );
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
          if ( value > Long.MIN_VALUE )
          {
            tempDataSet.getCursor( 0 ).setTimestamp( value );
          }
        }
        else if ( "CursorB".equals( instrKey ) )
        {
          final long value = safeParseLong( instrValue );
          if ( value > Long.MIN_VALUE )
          {
            tempDataSet.getCursor( 1 ).setTimestamp( value );
          }
        }
        else if ( instrKey.startsWith( "Cursor" ) )
        {
          final int idx = safeParseInt( instrKey.substring( 6 ) );
          final long pos = Long.parseLong( instrValue );
          if ( pos > Long.MIN_VALUE )
          {
            tempDataSet.getCursor( idx ).setTimestamp( pos );
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
    if ( rate == null )
    {
      throw new IOException( "Data file is corrupt?! Sample rate is not provided!" );
    }
    if ( ( channels == null ) || ( channels <= 0 ) || ( channels > 32 ) )
    {
      throw new IOException( "Data file is corrupt?! Channel count is not provided!" );
    }
    // Make sure the enabled channels are defined; if not defined, all channels
    // are enabled...
    if ( enabledChannels == null )
    {
      enabledChannels = -1; // = 0xffffffff
    }

    int[] values = new int[size];
    long[] timestamps = new long[size];

    try
    {
      for ( int i = 0; i < size; i++ )
      {
        final String[] dataPair = dataValues.get( i );

        values[i] = ( int )Long.parseLong( dataPair[0], 16 );
        timestamps[i] = Long.parseLong( dataPair[1], 10 ) & Long.MAX_VALUE;
      }
    }
    catch ( final NumberFormatException exception )
    {
      throw new IOException( "Invalid data encountered.", exception );
    }

    // Finally set the captured data, and notify all event listeners...
    capturedData = new CapturedData( values, timestamps, triggerPos, rate, channels, enabledChannels, absLen );

    return new DataSetImpl( capturedData, tempDataSet, false /* aRetainAnnotations */);
  }

  /**
   * Writes the data to the given writer.
   * 
   * @param aProject
   *          the project to write the settings for, cannot be <code>null</code>
   *          ;
   * @param aWriter
   *          the writer to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public static void write( final DataSet aDataSet, final Writer aWriter ) throws IOException
  {
    final BufferedWriter bw = new BufferedWriter( aWriter );

    final AcquisitionResult capturedData = aDataSet.getCapturedData();

    final Cursor[] cursors = aDataSet.getCursors();
    final boolean cursorsEnabled = aDataSet.isCursorsEnabled();

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
