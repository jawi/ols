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
package nl.lxtreme.ols.api.data;


import java.io.*;
import java.util.*;
import java.util.logging.*;


/**
 * Provides a wrapper for captured data in which the data can be annotated with
 * "any" kind of information, such as cursors, protocol decoding information,
 * and so on.
 * <p>
 * Data files will start with a header containing meta data marked by lines
 * starting with ";". The actual readout values will follow after the header. A
 * value is a logic level transition of one channel. The associated timestamp
 * since sample start (start has timestamp 0) is stored, too after a @
 * character. This is called compressed format. The handling of the data within
 * the class is the same. A value is 32bits long. The value is encoded in hex
 * and each value is followed by a new line.
 */
public final class AnnotatedData implements CapturedData
{
  // CONSTANTS

  /** The maximum number of cursors that can be set. */
  public static final int MAX_CURSORS = 10;
  /** The maximum number of channels. */
  public static final int MAX_CHANNELS = 32;

  private static final Logger LOG = Logger.getLogger( AnnotatedData.class.getName() );

  // VARIABLES

  /** the actual captured data */
  private volatile CapturedData capturedData;

  /** position of cursors */
  private final long[] cursorPositions;
  /** The labels of each channel. */
  private final String[] channelLabels;
  /** The individual annotations. */
  private final Map<Integer, ChannelAnnotations> annotations;

  /** cursors enabled status */
  private volatile boolean cursorEnabled;

  // CONSTRUCTORS

  /**
   * Creates a new AnnotatedData instance.
   */
  public AnnotatedData()
  {
    this.cursorPositions = new long[MAX_CURSORS];
    Arrays.fill( this.cursorPositions, Long.MIN_VALUE );

    this.channelLabels = new String[MAX_CHANNELS];
    Arrays.fill( this.channelLabels, "" );

    this.annotations = new HashMap<Integer, ChannelAnnotations>();
  }

  // METHODS

  /**
   * @param aChannelIdx
   * @param aStartIdx
   * @param aEndIdx
   * @param aData
   */
  public void addChannelAnnotation( final int aChannelIdx, final long aStartIdx, final long aEndIdx, final Object aData )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > this.channelLabels.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + this.channelLabels.length );
    }
    ChannelAnnotations annotations = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( annotations == null )
    {
      annotations = new ChannelAnnotations( aChannelIdx );
      this.annotations.put( Integer.valueOf( aChannelIdx ), annotations );
    }
    annotations.addAnnotation( aStartIdx, aEndIdx, aData );
  }

  /**
   * Calculates the time offset
   * 
   * @param time
   *          absolute sample number
   * @return time relative to data
   */
  public long calculateTime( final long aTime )
  {
    if ( this.capturedData.hasTriggerData() )
    {
      return aTime - this.capturedData.getTriggerPosition();
    }

    return aTime;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getAbsoluteLength()
   */
  @Override
  public long getAbsoluteLength()
  {
    return hasCapturedData() ? this.capturedData.getAbsoluteLength() : NOT_AVAILABLE;
  }

  /**
   * Returns the channel annotations.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0 &&
   *          < 32.
   * @return the channel annotations, can be <code>null</code>.
   */
  public ChannelAnnotation getChannelAnnotation( final int aChannelIdx, final long aTimeIndex )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > this.channelLabels.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + this.channelLabels.length );
    }

    final ChannelAnnotations channelAnnotations = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( channelAnnotations == null )
    {
      return null;
    }
    return channelAnnotations.getAnnotation( aTimeIndex );
  }

  /**
   * Returns the channel annotations.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the annotations for, >= 0 &&
   *          < 32.
   * @return the channel annotations, can be <code>null</code>.
   */
  public Iterator<ChannelAnnotation> getChannelAnnotations( final int aChannelIdx, final long aStartIdx,
      final long aEndIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > this.channelLabels.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + this.channelLabels.length );
    }

    final ChannelAnnotations channelAnnotations = this.annotations.get( Integer.valueOf( aChannelIdx ) );
    if ( channelAnnotations == null )
    {
      return Collections.<ChannelAnnotation> emptyList().iterator();
    }
    return channelAnnotations.getAnnotations( aStartIdx, aEndIdx );
  }

  /**
   * Returns the channel label.
   * 
   * @param aChannelIdx
   *          the index of the channel to retrieve the label for, >= 0 && < 32.
   * @return the channel's label, can be <code>null</code>.
   */
  public String getChannelLabel( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > this.channelLabels.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + this.channelLabels.length );
    }
    return this.channelLabels[aChannelIdx];
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannels()
   */
  @Override
  public int getChannels()
  {
    return hasCapturedData() ? this.capturedData.getChannels() : NOT_AVAILABLE;
  }

  /**
   * Get position of a cursor.
   * 
   * @param aCursorIdx
   *          the index of the cursor to set, should be >= 0 and < 10.
   * @return a cursor position, or Long.MIN_VALUE if not set.
   * @throws IllegalArgumentException
   *           in case an invalid cursor index was given.
   */
  public long getCursorPosition( final int aCursorIdx ) throws IllegalArgumentException
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > this.cursorPositions.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index: " + aCursorIdx + "! Should be between 0 and "
          + this.cursorPositions.length );
    }
    return this.cursorPositions[aCursorIdx];
  }

  /**
   * @return the cursorPositions
   */
  public long[] getCursorPositions()
  {
    return this.cursorPositions;
  }

  /**
   * Returns the (absolute) time value for the cursor indicated by the given
   * index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to return as time, should be >= 0 and <
   *          10.
   * @return the time value (in seconds), or -1.0 if the cursor is not
   *         available.
   */
  public Double getCursorTimeValue( final int aCursorIdx )
  {
    long cursorPos = getCursorPosition( aCursorIdx );
    if ( cursorPos > Long.MIN_VALUE )
    {
      return calculateTime( cursorPos ) / ( double )this.capturedData.getSampleRate();
    }
    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getDataAt(long)
   */
  @Override
  public int getDataAt( final long aAbs )
  {
    return hasCapturedData() ? this.capturedData.getDataAt( aAbs ) : NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getEnabledChannels()
   */
  @Override
  public int getEnabledChannels()
  {
    return hasCapturedData() ? this.capturedData.getEnabledChannels() : NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleIndex(long)
   */
  @Override
  public int getSampleIndex( final long aAbs )
  {
    return hasCapturedData() ? this.capturedData.getSampleIndex( aAbs ) : NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleRate()
   */
  @Override
  public int getSampleRate()
  {
    return hasCapturedData() ? this.capturedData.getSampleRate() : NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTimestamps()
   */
  @Override
  public long[] getTimestamps()
  {
    return hasCapturedData() ? this.capturedData.getTimestamps() : new long[0];
  }

  /**
   * Returns the trigger position, if it is available.
   * 
   * @return the available trigger position, or -1 if it is not available.
   */
  public long getTriggerPosition()
  {
    return hasCapturedData() && this.capturedData.hasTriggerData() ? this.capturedData.getTriggerPosition()
        : CapturedData.NOT_AVAILABLE;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getValues()
   */
  @Override
  public int[] getValues()
  {
    return hasCapturedData() ? this.capturedData.getValues() : new int[0];
  }

  /**
   * Returns whether any captured data is available.
   * 
   * @return <code>true</code> if there is captured data, <code>false</code>
   *         otherwise.
   */
  public boolean hasCapturedData()
  {
    return this.capturedData != null;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTimingData()
   */
  @Override
  public boolean hasTimingData()
  {
    return hasCapturedData() ? this.capturedData.hasTimingData() : false;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTriggerData()
   */
  @Override
  public boolean hasTriggerData()
  {
    return hasCapturedData() ? this.capturedData.hasTriggerData() : false;
  }

  /**
   * Returns whether or not the cursor data is enabled.
   * 
   * @return <code>true</code> if the cursors are enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isCursorsEnabled()
  {
    return this.cursorEnabled;
  }

  /**
   * Reads the data from a given reader.
   * 
   * @param aReader
   *          the reader to read the data from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void read( final Reader aReader ) throws IOException
  {
    int size = 0, r = -1, channels = 32, enabledChannels = -1;
    long t = -1;

    long[] cursorPositions = new long[10];
    Arrays.fill( cursorPositions, Long.MIN_VALUE );

    boolean cursors = false;
    boolean compressed = false;
    long absLen = 0;
    String line;

    final BufferedReader br = new BufferedReader( aReader );
    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Parsing OLS captured data from stream..." );
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
        final int idx = Integer.parseInt( line.substring( 7, 8 ) );
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

    long absoluteLength;
    int[] values;
    long[] timestamps;

    if ( compressed )
    {
      // new compressed file format
      absoluteLength = absLen;
      values = new int[size];
      timestamps = new long[size];
      try
      {
        for ( int i = 0; ( i < values.length ) && ( line != null ); i++ )
        {
          values[i] = Integer.parseInt( line.substring( 0, 4 ), 16 ) << 16
              | Integer.parseInt( line.substring( 4, 8 ), 16 );
          timestamps[i] = Long.parseLong( line.substring( 9 ) );
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

      absoluteLength = size;
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
      values = new int[count];
      timestamps = new long[count];
      timestamps[0] = 0;
      values[0] = tmpValues[0];
      tmp = tmpValues[0];
      count = 1;
      for ( int i = 0; i < tmpValues.length; i++ )
      {
        if ( tmp != tmpValues[i] )
        {
          // store only transitions
          timestamps[count] = i;
          values[count] = tmpValues[i];
          count++;
        }
        tmp = tmpValues[i];
      }
    }

    this.capturedData = new CapturedDataImpl( values, timestamps, t, r, channels, enabledChannels, absoluteLength );

    System.arraycopy( cursorPositions, 0, this.cursorPositions, 0, cursorPositions.length );
    this.cursorEnabled = cursors;

    br.close();
  }

  /**
   * Sets the captured data.
   * 
   * @param aCapturedData
   *          the captured data to set, may be <code>null</code>.
   */
  public void setCapturedData( final CapturedData aCapturedData )
  {
    this.capturedData = aCapturedData;
    this.annotations.clear();
  }

  /**
   * @param aChannelIdx
   *          the index of the channel to set the label for, >= 0 && < 32;
   * @param aAnnotations
   */
  public void setChannelAnnotations( final int aChannelIdx, final ChannelAnnotations aAnnotations )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > this.channelLabels.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + this.channelLabels.length );
    }
    this.annotations.put( Integer.valueOf( aChannelIdx ), aAnnotations );
  }

  /**
   * Sets the channel label.
   * 
   * @param aChannelIdx
   *          the index of the channel to set the label for, >= 0 && < 32;
   * @param aLabel
   *          the label to set, may be <code>null</code>.
   */
  public void setChannelLabel( final int aChannelIdx, final String aLabel )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx > this.channelLabels.length - 1 ) )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + aChannelIdx + "! Should be between 0 and "
          + this.channelLabels.length );
    }
    this.channelLabels[aChannelIdx] = aLabel;
  }

  /**
   * Sets whether or not the cursor data is enabled.
   * 
   * @param aCursorEnabled
   *          <code>true</code> to the enable the cursor data,
   *          <code>false</code> otherwise.
   */
  public void setCursorEnabled( final boolean aCursorEnabled )
  {
    this.cursorEnabled = aCursorEnabled;
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
   * Writes the data to the given writer.
   * 
   * @param aWriter
   *          the writer to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  public void write( final Writer aWriter ) throws IOException
  {
    final BufferedWriter bw = new BufferedWriter( aWriter );

    try
    {
      final int[] values = this.capturedData.getValues();
      final long[] timestamps = this.capturedData.getTimestamps();

      bw.write( ";Size: " + values.length );
      bw.newLine();
      bw.write( ";Rate: " + this.capturedData.getSampleRate() );
      bw.newLine();
      bw.write( ";Channels: " + this.capturedData.getChannels() );
      bw.newLine();
      bw.write( ";EnabledChannels: " + this.capturedData.getEnabledChannels() );
      bw.newLine();
      if ( this.capturedData.hasTriggerData() )
      {
        bw.write( ";TriggerPosition: " + this.capturedData.getTriggerPosition() );
        bw.newLine();
      }
      bw.write( ";Compressed: true" );
      bw.newLine();
      bw.write( ";AbsoluteLength: " + this.capturedData.getAbsoluteLength() );
      bw.newLine();

      for ( int i = 0; i < values.length; i++ )
      {
        final String hexVal = Integer.toHexString( values[i] );
        bw.write( "00000000".substring( hexVal.length() ) + hexVal );
        bw.write( "@" + timestamps[i] );
        bw.newLine();
      }
      bw.write( ";CursorEnabled: " + this.cursorEnabled );
      bw.newLine();
      for ( int i = 0; i < this.cursorPositions.length; i++ )
      {
        bw.write( ";Cursor" + i + ": " + this.cursorPositions[i] );
        bw.newLine();
      }
    }
    finally
    {
      bw.flush();
    }
  }
}
