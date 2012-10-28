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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


/**
 * Denotes sample data acquired from a device.
 */
public interface AcquisitionData
{
  // METHODS

  /**
   * Returns the absolute length of the captured data, or, in other words, the
   * largest available time value as returned by {@link #getTimestamps()}.
   * 
   * @return the absolute length, >= 0.
   */
  long getAbsoluteLength();

  /**
   * Returns the total number of channels in the sample data, and therefore, the
   * width of a single sample in bits.
   * <p>
   * The result of this method does not indicate which bits in a sample have
   * meaningful data. This information can be obtained by using the bitmask
   * returned by {@link #getEnabledChannels()}.
   * </p>
   * 
   * @return the channel count, >= 0.
   * @see #getEnabledChannels()
   */
  int getChannelCount();

  /**
   * Returns the channel labels describing what kind of data a channel is
   * supposed to represent.
   * <p>
   * The array returned by this method is always equal to the value returned by
   * {@link #getChannelCount()}.
   * </p>
   * <p>
   * Users of this method <b>may</b> change its contents to change a channel
   * label. Any kind of value is allowed, even <code>null</code> which denotes
   * that the default label for a channel should be used.
   * </p>
   * 
   * @return an array of channel labels, never <code>null</code>.
   */
  String[] getChannelLabels();

  /**
   * Returns the number of channels in the sample data.
   * 
   * @return the channel count, >= 0.
   * @deprecated use {@link #getChannelCount()}
   */
  @Deprecated
  int getChannels();

  /**
   * Returns a bitmask of enabled channels in the sample data.
   * 
   * @return a bitmask of enabled channels, for example, 0xFF for the first 8
   *         channels.
   */
  int getEnabledChannels();

  /**
   * Returns the sample index from the given absolute time value.
   * <p>
   * The result of this method is always an index <em>N</em> such that:
   * <tt>timestamps[N-1] &lt; timestamps[N] &lt; timestamps[N+1]</tt> even if
   * the given time value is not contained in the results of
   * {@link #getTimestamps()}.
   * </p>
   * 
   * @param aTimeValue
   *          the (absolute) time value to convert to a sample index.
   * @return the sample number <em>before</em> the selected absolute time.
   */
  int getSampleIndex( final long aTimeValue );

  /**
   * Returns the sample rate in which this data was captured.
   * 
   * @return a sample rate in hertz (Hz).
   */
  int getSampleRate();

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
   * <p>
   * Although this method returns an array of mutable values, users of this
   * method are supposed to modify any of the values.
   * </p>
   * 
   * @return the time stamps, as array of long values.
   * @see #getValues()
   */
  long[] getTimestamps();

  /**
   * Returns the trigger position, as (absolute) time-value.
   * 
   * @return a value representing the trigger position in time, or <tt>-1</tt>
   *         if no trigger position is available.
   */
  long getTriggerPosition();

  /**
   * Returns the actual sample values.
   * <p>
   * NOTE: the length of this array <b>must</b> be equal to the length of the
   * array returned by {@link #getTimestamps()}!
   * </p>
   * <p>
   * Although this method returns an array of mutable values, users of this
   * method are supposed to modify any of the values.
   * </p>
   * 
   * @return the sample values, as array of integers.
   * @see #getTimestamps()
   */
  int[] getValues();

  /**
   * Returns whether or not the object contains timing data such that the values
   * returned by {@link #getTimestamps()} represent time values. If no timing
   * data is available, the values of {@link #getTimestamps()} are supposed to
   * return sample indexes.
   * 
   * @return <code>true</code> when timing data is available, <code>false</code>
   *         if no timing data is available.
   */
  boolean hasTimingData();

  /**
   * Returns whether or not the object contains trigger data, such that
   * {@link #getTriggerPosition()} returns a valid timestamp.
   * 
   * @return <code>true</code> when trigger data is available,
   *         <code>false</code> otherwise.
   */
  boolean hasTriggerData();

}
