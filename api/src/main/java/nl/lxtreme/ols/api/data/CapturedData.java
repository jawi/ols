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
  // INNER TYPES

  /** indicates that rate or trigger position are not available */
  public final static int NOT_AVAILABLE = -1;

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
   * Returns the trigger position, as array index.
   * 
   * @return an array index representing the trigger position, >= 0.
   */
  public abstract int getTriggerIndex();

  /**
   * Returns the trigger position, as (array) time-value.
   * 
   * @return a value representing the trigger position, >= 0.
   */
  public abstract long getTriggerTimePosition();

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
