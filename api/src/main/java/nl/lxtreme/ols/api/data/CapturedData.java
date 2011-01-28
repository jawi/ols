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
 * @author jawi
 */
public interface CapturedData
{
  // CONSTANTS

  /** indicates that rate or trigger position are not available */
  public final static int NOT_AVAILABLE = -1;

  /** The maximum number of cursors that can be set. */
  public static final int MAX_CURSORS = 10;
  /** The maximum number of channels. */
  public static final int MAX_CHANNELS = 32;
  /** The number of channels per block. */
  public static final int CHANNELS_PER_BLOCK = 8;
  /** The maximum number of blocks. */
  public static final int MAX_BLOCKS = MAX_CHANNELS / CHANNELS_PER_BLOCK;

  // METHODS

  /**
   * Returns the absolute length of the captured data.
   * 
   * @return the absolute length, >= 0.
   */
  public abstract long getAbsoluteLength();

  /**
   * Returns the number of channels in the sample data.
   * 
   * @return the channel count, >= 0.
   */
  public abstract int getChannels();

  /**
   * Returns the number of enabled channels in the sample data.
   * 
   * @return the enabled channels, >= 0.
   */
  public abstract int getEnabledChannels();

  /**
   * Returns the sample index from the given absolute time value.
   * 
   * @param aTimeValue
   *          the (absolute) time value to convert to a sample index.
   * @return the sample number <em>before</b> the selected absolute time.
   */
  public abstract int getSampleIndex( final long aTimeValue );

  /**
   * Returns the sample rate in which this data was captured.
   * 
   * @return a sample rate in hertz (Hz).
   */
  public abstract int getSampleRate();

  /**
   * Returns the time stamp data.
   * 
   * @return the time stamps, as array of long values.
   */
  public abstract long[] getTimestamps();

  /**
   * Returns the trigger position, as (absolute) time-value.
   * 
   * @return a value representing the trigger position in time.
   */
  public abstract long getTriggerPosition();

  /**
   * @return the values
   */
  public abstract int[] getValues();

  /**
   * Returns wether or not the object contains timing data
   * 
   * @return <code>true</code> when timing data is available
   */
  public abstract boolean hasTimingData();

  /**
   * Returns wether or not the object contains trigger data
   * 
   * @return <code>true</code> when trigger data is available
   */
  public abstract boolean hasTriggerData();

}
