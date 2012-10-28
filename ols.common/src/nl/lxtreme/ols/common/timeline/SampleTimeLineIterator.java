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
package nl.lxtreme.ols.common.timeline;


import java.util.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a {@link SampleTimeLineIterator} iterator, which allows you to
 * iterate over the timestamps of an {@link AcquisitionData} and obtain data
 * values at specific moments in time from it.
 */
public class SampleTimeLineIterator extends AbstractTimeLineIterator
{
  // VARIABLES

  private final int[] values;
  private final long[] timestamps;
  private final boolean inverted;
  private final int sampleMask;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SampleTimeLineIterator} instance.
   * 
   * @param aDataSet
   *          the data set to use, cannot be <code>null</code>;
   * @param aInverted
   *          <code>true</code> if the sample value should be bitwise inverted
   *          before return;
   * @param aSampleMask
   *          the mask to apply to the sample value before return.
   */
  public SampleTimeLineIterator( final AcquisitionData aDataSet, final boolean aInverted, final int aSampleMask )
  {
    this( aDataSet, aInverted, aSampleMask, aDataSet.getTimestamps()[0], aDataSet.getAbsoluteLength() );
  }

  /**
   * Creates a new {@link SampleTimeLineIterator} instance.
   * 
   * @param aDataSet
   *          the data set to use, cannot be <code>null</code>.
   * @param aInverted
   *          <code>true</code> if the sample value should be bitwise inverted
   *          before return;
   * @param aSampleMask
   *          the mask to apply to the sample value before return;
   * @param aStartIdx
   *          the index of the start timestamp, >= 0;
   * @param aEndIdx
   *          the index of the ending timestamp, >= 0.
   */
  public SampleTimeLineIterator( final AcquisitionData aDataSet, final boolean aInverted, final int aSampleMask,
      final int aStartIdx, final int aEndIdx )
  {
    this( aDataSet, aInverted, aSampleMask, aDataSet.getTimestamps()[aStartIdx], aDataSet.getTimestamps()[aEndIdx] );
  }

  /**
   * Creates a new {@link SampleTimeLineIterator} instance.
   * 
   * @param aDataSet
   *          the data set to use, cannot be <code>null</code>.
   * @param aInverted
   *          <code>true</code> if the sample value should be bitwise inverted
   *          before return;
   * @param aSampleMask
   *          the mask to apply to the sample value before return;
   * @param aStartTime
   *          the start timestamp, >= 0L;
   * @param aEndTime
   *          the end timestamp, >= 0L.
   */
  public SampleTimeLineIterator( final AcquisitionData aDataSet, final boolean aInverted, final int aSampleMask,
      final long aStartTime, final long aEndTime )
  {
    this( aDataSet.getValues(), aDataSet.getTimestamps(), aInverted, aSampleMask, aStartTime, aEndTime );
  }

  /**
   * Creates a new {@link SampleTimeLineIterator} instance.
   * 
   * @param aValues
   *          the data values to use, cannot be <code>null</code>;
   * @param aTimestamps
   *          the timestamps to use, cannot be <code>null</code>;
   * @param aInverted
   *          <code>true</code> if the sample value should be bitwise inverted
   *          before return;
   * @param aSampleMask
   *          the mask to apply to the sample value before return;
   * @param aStartTime
   *          the start timestamp, >= 0L;
   * @param aEndTime
   *          the end timestamp, >= 0L.
   */
  SampleTimeLineIterator( final int[] aValues, final long[] aTimestamps, final boolean aInverted,
      final int aSampleMask, final long aStartTime, final long aEndTime )
  {
    super( aStartTime, aEndTime );

    this.values = aValues;
    this.timestamps = aTimestamps;
    this.inverted = aInverted;
    this.sampleMask = aSampleMask;
  }

  // METHODS

  /**
   * Finds the sample index of the given timestamp value.
   * <p>
   * Note the given time value is <em>not per se</em> equal to
   * <code>timestamps[result]</code>; as this method returns the index on which
   * the given time value should occur in the given array of timestamps!
   * </p>
   * 
   * @param aTimestamps
   *          the timestamps to find the given time value in, cannot be
   *          <code>null</code>;
   * @param aTimeValue
   *          the time value to search the corresponding index for, >= 0.
   * @return a sample index, >= 0.
   */
  private static final int findSampleIndex( final long[] aTimestamps, final long aTimeValue )
  {
    int k = Arrays.binarySearch( aTimestamps, aTimeValue );
    if ( k < 0 )
    {
      k = -( k + 1 );
    }
    return k;
  }

  /**
   * Returns the sample value for the given time stamp.
   * 
   * @param aTimeValue
   *          the time stamp to return the data value for.
   * @return the data value of the sample index right before the given time
   *         value.
   */
  @Override
  public int peekValue( final long aTimeValue )
  {
    int k = findSampleIndex( this.timestamps, aTimeValue );

    int sample = ( k == 0 ) ? this.values[0] : this.values[k - 1];
    if ( this.inverted )
    {
      sample = ~sample;
    }
    return sample & this.sampleMask;
  }
}
