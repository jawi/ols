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
package nl.lxtreme.ols.api;


import java.io.*;
import java.util.*;
import java.util.logging.*;


/**
 * CapturedData encapsulates the data obtained by the analyzer during a single run.
 * It also provides a method for saving the data to a file.
 * <p>
 * Data files will start with a header containing meta data marked by lines starting with ";". The actual readout values
 * will follow after the header. A value is a logic level transition of one channel. The associated timestamp since
 * sample start (start has timestamp 0) is stored, too after a @ character. This is called compressed format. The
 * handling of the data within the class is the same. A value is 32bits long. The value is encoded in hex and each value
 * is followed by a new line.
 * <p>
 * In the java code each transition is represented by an integer together with a timestamp represented by a long value.
 * 
 * @version 0.7
 * @author Michael "Mr. Sump" Poppitz
 */
public class CapturedData
{
  // CONSTANTS

  /** indicates that rate or trigger position are not available */
  public final static int     NOT_AVAILABLE = -1;

  private static final Logger LOG           = Logger.getLogger( CapturedData.class.getName() );

  // VARIABLES

  /** captured values */
  public final int[]          values;

  /** timestamp values in samples count from start */
  public final long[]         timestamps;

  /** position of trigger as index of values */
  public final long           triggerPosition;

  /** sampling rate in Hz */
  public final int            rate;

  /** number of channels (1-32) */
  public final int            channels;

  /** bit map of enabled channels */
  public final int            enabledChannels;

  /** absolute sample length */
  public final long           absoluteLength;

  /* position of cursors */
  public final long[]         cursorPositions;

  /** cursors enabled status */
  public boolean              cursorEnabled;

  // CONSTRUCTORS

  /**
   * Constructs CapturedData based on the data read from the given file.
   * 
   * @param file
   *          file to read captured data from
   * @throws IOException
   *           when reading from file failes
   */
  public CapturedData( final File file ) throws IOException
  {
    int size = 0, r = -1, channels = 32, enabledChannels = -1;
    long t = -1;

    long[] cursorPositions = new long[10];
    Arrays.fill( cursorPositions, -1L );

    boolean cursors = false;
    boolean compressed = false;
    long absLen = 0;
    String line;

    final BufferedReader br = new BufferedReader( new FileReader( file ) );
    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Parsing '" + file + "' as OLS captured data..." );
    }

    do
    {
      line = br.readLine();
      if ( line == null )
      {
        throw new IOException( "File appears to be corrupt." );
      }
      else if ( line.startsWith( ";Size: " ) )
      {
        size = Integer.parseInt( line.substring( 7 ) );
      }
      else if ( line.startsWith( ";Rate: " ) )
      {
        r = Integer.parseInt( line.substring( 7 ) );
      }
      else if ( line.startsWith( ";Channels: " ) )
      {
        channels = Integer.parseInt( line.substring( 11 ) );
      }
      else if ( line.startsWith( ";TriggerPosition: " ) )
      {
        t = Long.parseLong( line.substring( 18 ) );
      }
      else if ( line.startsWith( ";EnabledChannels: " ) )
      {
        enabledChannels = Integer.parseInt( line.substring( 18 ) );
      }
      else if ( line.startsWith( ";CursorA: " ) )
      {
        cursorPositions[0] = Long.parseLong( line.substring( 10 ) );
      }
      else if ( line.startsWith( ";CursorB: " ) )
      {
        cursorPositions[1] = Long.parseLong( line.substring( 10 ) );
      }
      else if ( line.matches( ";Cursor(\\d+): (\\d+)" ) )
      {
        final int idx = Integer.parseInt( line.substring( 7, 9 ) );
        final long pos = Long.parseLong( line.substring( 10 ) );
        cursorPositions[idx] = pos;
      }
      else if ( line.startsWith( ";CursorEnabled: " ) )
      {
        cursors = Boolean.parseBoolean( line.substring( 16 ) );
      }
      else if ( line.startsWith( ";Compressed: " ) )
      {
        compressed = Boolean.parseBoolean( line.substring( 13 ) );
      }
      else if ( line.startsWith( ";AbsoluteLength: " ) )
      {
        absLen = Long.parseLong( line.substring( 17 ) );
      }
    }
    while ( line.startsWith( ";" ) );

    if ( compressed )
    {
      // new compressed file format
      this.absoluteLength = absLen;
      this.values = new int[size];
      this.timestamps = new long[size];
      try
      {
        for ( int i = 0; ( i < this.values.length ) && ( line != null ); i++ )
        {
          this.values[i] = Integer.parseInt( line.substring( 0, 4 ), 16 ) << 16
              | Integer.parseInt( line.substring( 4, 8 ), 16 );
          this.timestamps[i] = Long.parseLong( line.substring( 9 ) );
          line = br.readLine();
        }
      }
      catch ( final NumberFormatException E )
      {
        throw new IOException( "Invalid data encountered." );
      }
    }
    else
    {
      // old sample based file format
      if ( ( size <= 0 ) || ( size > 1024 * 256 ) )
      {
        throw new IOException( "Invalid size encountered." );
      }

      this.absoluteLength = size;
      final int[] tmpValues = new int[size];
      try
      {
        // read all values
        for ( int i = 0; ( i < tmpValues.length ) && ( line != null ); i++ )
        {
          // TODO: modify to work with all channel counts up to 32
          if ( channels > 16 )
          {
            tmpValues[i] = Integer.parseInt( line.substring( 0, 4 ), 16 ) << 16
                | Integer.parseInt( line.substring( 4, 8 ), 16 );
          }
          else
          {
            tmpValues[i] = Integer.parseInt( line.substring( 0, 4 ), 16 );
          }
          line = br.readLine();
        }
      }
      catch ( final NumberFormatException E )
      {
        throw new IOException( "Invalid data encountered." );
      }

      int count = 0;
      int tmp = tmpValues[0];

      // calculate transitions
      for ( final int tmpValue : tmpValues )
      {
        if ( tmp != tmpValue )
        {
          count++;
        }
        tmp = tmpValue;
      }
      count++;

      // compress
      this.values = new int[count];
      this.timestamps = new long[count];
      this.timestamps[0] = 0;
      this.values[0] = tmpValues[0];
      tmp = tmpValues[0];
      count = 1;
      for ( int i = 0; i < tmpValues.length; i++ )
      {
        if ( tmp != tmpValues[i] )
        {
          // store only transitions
          this.timestamps[count] = i;
          this.values[count] = tmpValues[i];
          count++;
        }
        tmp = tmpValues[i];
      }
    }

    this.triggerPosition = t;
    this.rate = r;
    this.channels = channels;
    this.enabledChannels = enabledChannels;
    this.cursorPositions = new long[10];
    System.arraycopy( cursorPositions, 0, this.cursorPositions, 0, cursorPositions.length );
    this.cursorEnabled = cursors;

    br.close();
  }

  /**
   * Constructs CapturedData based on the given absolute sampling data.
   * 
   * @param values
   *          32bit values as read from device
   * @param triggerPosition
   *          position of trigger as index of values array
   * @param rate
   *          sampling rate (may be set to <code>NOT_AVAILABLE</code>)
   * @param channels
   *          number of used channels
   * @param enabledChannels
   *          bit mask identifying used channels
   */
  public CapturedData( final int[] values, final long triggerPosition, final int rate, final int channels,
      final int enabledChannels )
  {
    this.triggerPosition = triggerPosition;
    this.rate = rate;
    this.channels = channels;
    this.enabledChannels = enabledChannels;
    this.cursorPositions = new long[10];
    Arrays.fill( this.cursorPositions, -1L );
    // calculate transitions
    int tmp = values[0];
    int count = 1; // first value is the initial value at time 0
    for ( final int value : values )
    {
      if ( tmp != value )
      {
        count++;
      }
      tmp = value;
    }
    this.timestamps = new long[count];
    this.values = new int[count];
    this.timestamps[0] = 0;
    this.values[0] = values[0];
    tmp = values[0];
    count = 1;
    for ( int i = 0; i < values.length; i++ )
    {
      if ( tmp != values[i] )
      {
        // store only transitions
        this.timestamps[count] = i;
        this.values[count] = values[i];
        count++;
      }
      tmp = values[i];
    }
    this.absoluteLength = values.length;
  }

  /**
   * Constructs CapturedData based on the given compressed sampling data.
   * 
   * @param values
   *          32bit values as read from device
   * @param timestamps
   *          timstamps in number of samples since sample start
   * @param triggerPosition
   *          position of trigger as index of values array
   * @param rate
   *          sampling rate (may be set to <code>NOT_AVAILABLE</code>)
   * @param channels
   *          number of used channels
   * @param enabledChannels
   *          bit mask identifying used channels
   * @param absLen
   *          absolute number of samples
   */
  public CapturedData( final int[] values, final long[] timestamps, final long triggerPosition, final int rate,
      final int channels, final int enabledChannels, final long absLen )
  {
    this.values = values;
    this.timestamps = timestamps;
    this.triggerPosition = triggerPosition;
    this.rate = rate;
    this.channels = channels;
    this.enabledChannels = enabledChannels;
    this.cursorPositions = new long[10];
    Arrays.fill( this.cursorPositions, -1L );
    this.absoluteLength = absLen;
  }

  // METHODS

  /**
   * Get position of a cursor.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set, should be >= 0 and < 10.
   * @return sample position
   * @throws IllegalArgumentException
   *           in case an invalid cursor index was given.
   */
  public long getCursorPosition( final int aCursorIdx ) throws IllegalArgumentException
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > this.cursorPositions.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index! Should be between 0 and "
          + this.cursorPositions.length );
    }
    return this.cursorPositions[aCursorIdx];
  }

  /**
   * return the data value at a specified absolute time offset
   * 
   * @param abs
   *          absolute time value
   * @return data value
   */
  public int getDataAt( final long abs )
  {
    return this.values[getSampleIndex( abs )];
  }

  /**
   * calculate index number from absolute time
   * 
   * @param abs
   *          absolute time value
   * @return sample number before selected absolute time
   */
  public int getSampleIndex( final long abs )
  {
    int i;
    for ( i = 1; i < this.timestamps.length; i++ )
    {
      if ( abs < this.timestamps[i] )
      {
        break;
      }
    }
    return i - 1;
  }

  /**
   * Returns wether or not the object contains timing data
   * 
   * @return <code>true</code> when timing data is available
   */
  public boolean hasTimingData()
  {
    return ( this.rate != NOT_AVAILABLE );
  }

  /**
   * Returns wether or not the object contains trigger data
   * 
   * @return <code>true</code> when trigger data is available
   */
  public boolean hasTriggerData()
  {
    return ( this.triggerPosition != NOT_AVAILABLE );
  }

  /**
   * Sets a cursor position.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set, should be >= 0 and < 10;
   * @param aCursorPosition
   *          the actual cursor position to set.
   * @throws IllegalArgumentException
   *           in case an invalid cursor index was given.
   */
  public void setCursorPosition( final int aCursorIdx, final long aCursorPosition ) throws IllegalArgumentException
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > this.cursorPositions.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index! Should be between 0 and "
          + this.cursorPositions.length );
    }
    this.cursorPositions[aCursorIdx] = aCursorPosition;
  }

  /**
   * Writes device data to given file.
   * 
   * @param file
   *          file to write to
   * @throws IOException
   *           when writing to file failes
   */
  public void writeToFile( final File file ) throws IOException
  {
    try
    {
      final BufferedWriter bw = new BufferedWriter( new FileWriter( file ) );

      bw.write( ";Size: " + this.values.length );
      bw.newLine();
      bw.write( ";Rate: " + this.rate );
      bw.newLine();
      bw.write( ";Channels: " + this.channels );
      bw.newLine();
      bw.write( ";EnabledChannels: " + this.enabledChannels );
      bw.newLine();
      if ( this.triggerPosition >= 0 )
      {
        bw.write( ";TriggerPosition: " + this.triggerPosition );
        bw.newLine();
      }
      bw.write( ";CursorEnabled: " + this.cursorEnabled );
      bw.newLine();
      for ( int i = 0; i < this.cursorPositions.length; i++ )
      {
        bw.write( ";Cursor" + i + ": " + this.cursorPositions[i] );
        bw.newLine();
      }
      bw.write( ";Compressed: true" );
      bw.newLine();
      bw.write( ";AbsoluteLength: " + this.absoluteLength );
      bw.newLine();

      for ( int i = 0; i < this.values.length; i++ )
      {
        final String hexVal = Integer.toHexString( this.values[i] );
        bw.write( "00000000".substring( hexVal.length() ) + hexVal );
        bw.write( "@" + this.timestamps[i] );
        bw.newLine();
      }
      bw.close();
    }
    catch ( final Exception E )
    {
      E.printStackTrace( System.out );
    }
  }
}
