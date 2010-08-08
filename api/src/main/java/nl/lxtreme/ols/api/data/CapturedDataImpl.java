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


/**
 * CapturedData encapsulates the data obtained by the analyzer during a single
 * run.
 * <p>
 * In the java code each transition is represented by an integer together with a
 * timestamp represented by a long value.
 * 
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public class CapturedDataImpl implements CapturedData
{
  // VARIABLES

  /** captured values */
  private final int[] values;

  /** timestamp values in samples count from start */
  private final long[] timestamps;

  /** position of trigger as index of values */
  private final long triggerPosition;

  /** sampling rate in Hz */
  private final int rate;

  /** number of channels (1-32) */
  private final int channels;

  /** bit map of enabled channels */
  private final int enabledChannels;

  /** absolute sample length */
  private final long absoluteLength;

  // CONSTRUCTORS

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
  public CapturedDataImpl( final int[] values, final long triggerPosition, final int rate, final int channels,
      final int enabledChannels )
  {
    this.triggerPosition = triggerPosition;
    this.rate = rate;
    this.channels = channels;
    this.enabledChannels = enabledChannels;

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
  public CapturedDataImpl( final int[] values, final long[] timestamps, final long triggerPosition, final int rate,
      final int channels, final int enabledChannels, final long absLen )
  {
    this.values = values;
    this.timestamps = timestamps;
    this.triggerPosition = triggerPosition;
    this.rate = rate;
    this.channels = channels;
    this.enabledChannels = enabledChannels;
    this.absoluteLength = absLen;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getAbsoluteLength()
   */
  @Override
  public final long getAbsoluteLength()
  {
    return this.absoluteLength;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getChannels()
   */
  @Override
  public final int getChannels()
  {
    return this.channels;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getDataAt(long)
   */
  @Override
  public final int getDataAt( final long abs )
  {
    return this.values[getSampleIndex( abs )];
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getEnabledChannels()
   */
  @Override
  public final int getEnabledChannels()
  {
    return this.enabledChannels;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleIndex(long)
   */
  @Override
  public final int getSampleIndex( final long abs )
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
   * @see nl.lxtreme.ols.api.data.CapturedData#getSampleRate()
   */
  @Override
  public final int getSampleRate()
  {
    return this.rate;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTimestamps()
   */
  @Override
  public final long[] getTimestamps()
  {
    return this.timestamps;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getTriggerPosition()
   */
  @Override
  public final long getTriggerPosition()
  {
    return this.triggerPosition;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#getValues()
   */
  @Override
  public final int[] getValues()
  {
    return this.values;
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTimingData()
   */
  @Override
  public final boolean hasTimingData()
  {
    return ( this.rate != NOT_AVAILABLE );
  }

  /**
   * @see nl.lxtreme.ols.api.data.CapturedData#hasTriggerData()
   */
  @Override
  public final boolean hasTriggerData()
  {
    return ( this.triggerPosition != NOT_AVAILABLE );
  }
}
