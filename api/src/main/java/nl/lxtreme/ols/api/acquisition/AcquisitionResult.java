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
package nl.lxtreme.ols.api.acquisition;


/**
 * Denotes a concrete result of a single acquisition.
 */
public interface AcquisitionResult
{
  // METHODS

  /**
   * Returns the absolute length of the captured data, or, in other words, the
   * largest available timestamp plus some padding to make the last sample
   * visible.
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
   * Returns a bitmask of enabled channels in the sample data.
   * 
   * @return a bitmask of enabled channels, for example, 0xFF for the first 8
   *         channels.
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
   * Returns the time stamps of the individual samples.
   * <p>
   * The time values returned should represent an ever increasing time line. So,
   * <tt>timestamp[n-1] &lt; timestamp[n] &lt; timestamp[n+1]</tt> for all
   * elements of the returned array.
   * </p>
   * <p>
   * NOTE: the length of this array <b>must</b> be equal to the length of the
   * array returned by {@link #getValues()}!
   * </p>
   * 
   * @return the time stamps, as array of long values.
   * @see #getValues()
   */
  public abstract long[] getTimestamps();

  /**
   * Returns the trigger position, as (absolute) time-value.
   * 
   * @return a value representing the trigger position in time.
   */
  public abstract long getTriggerPosition();

  /**
   * Returns the actual sample values.
   * <p>
   * NOTE: the length of this array <b>must</b> be equal to the length of the
   * array returned by {@link #getTimestamps()}!
   * </p>
   * 
   * @return the sample values, as array of integers.
   * @see #getTimestamps()
   */
  public abstract int[] getValues();

  /**
   * Returns wether or not the object contains timing data
   * 
   * @return <code>true</code> when timing data is available
   */
  public abstract boolean hasTimingData();

  /**
   * Returns whether or not the object contains trigger data
   * 
   * @return <code>true</code> when trigger data is available
   */
  public abstract boolean hasTriggerData();

}
