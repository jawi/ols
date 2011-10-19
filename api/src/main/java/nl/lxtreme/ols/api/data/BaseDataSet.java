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
package nl.lxtreme.ols.api.data;


import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;


/**
 * Provides a base data set implementation.
 * 
 * @param <DATA>
 *          the actual data entity of this base data set.
 */
public class BaseDataSet<DATA extends BaseData<DATA>>
{
  // VARIABLES

  private final List<DATA> data;
  private final int startOfDecode;
  private final int endOfDecode;
  private final boolean timingDataPresent;
  private final int sampleRate;
  private final boolean triggerDataPresent;
  private final long triggerPosition;
  private final long[] timestamps;

  // CONSTRUCTORS

  /**
   * Creates a new BaseDataSet.
   * 
   * @param aStartOfDecodeIdx
   *          the sample index denoting the start of this data set;
   * @param aEndOfDecodeIdx
   *          the sample index denoting the end of this data set;
   * @param aData
   *          the actual acquisition results used in this data set.
   */
  public BaseDataSet( final int aStartOfDecodeIdx, final int aEndOfDecodeIdx, final AcquisitionResult aData )
  {
    this.data = new ArrayList<DATA>();

    this.startOfDecode = aStartOfDecodeIdx;
    this.endOfDecode = aEndOfDecodeIdx;

    this.timingDataPresent = aData.hasTimingData();
    this.sampleRate = aData.getSampleRate();

    this.triggerDataPresent = aData.hasTriggerData();
    this.triggerPosition = aData.getTriggerPosition();

    this.timestamps = aData.getTimestamps();
  }

  // METHODS

  /**
   * Returns the (decoded) data in this data set.
   * 
   * @return the data, never <code>null</code>.
   */
  public final List<DATA> getData()
  {
    return this.data;
  }

  /**
   * Returns the sample (array) index on which the decoding is stopped.
   * 
   * @return a sample (array) index, >= 0.
   */
  public final int getEndOfDecode()
  {
    return this.endOfDecode;
  }

  /**
   * @return the sampleRate
   */
  public final int getSampleRate()
  {
    return this.sampleRate;
  }

  /**
   * Returns the sample (array) index on which the decoding is started.
   * 
   * @return a sample (array) index, >= 0.
   */
  public final int getStartOfDecode()
  {
    return this.startOfDecode;
  }

  /**
   * Returns the time-value for the given sample index, taking the (optional)
   * trigger position into consideration.
   * 
   * @param aSampleIdx
   *          the sample index to return as (absolute) time value.
   * @return a real time value, never <code>null</code>.
   */
  public final double getTime( final int aSampleIdx )
  {
    long time = this.timestamps[aSampleIdx];
    if ( this.triggerDataPresent )
    {
      time -= this.triggerPosition;
    }
    if ( this.timingDataPresent )
    {
      return time / ( double )this.sampleRate;
    }

    return time;
  }

  /**
   * Returns whether this data set is empty or not.
   * 
   * @return <code>true</code> if this data set is empty, <code>false</code>
   *         otherwise.
   */
  public final boolean isEmpty()
  {
    return this.data.isEmpty();
  }

  /**
   * Adds data to this dataset.
   * 
   * @param aData
   *          the data to add, cannot be <code>null</code>.
   */
  protected final void addData( final DATA aData )
  {
    this.data.add( aData );
  }

  /**
   * Returns the current size of this data set.
   * 
   * @return a data set size, >= 0.
   */
  protected final int size()
  {
    return this.data.size();
  }

  /**
   * Sorts the data according to the {@link Comparable} implementation of DATA.
   */
  protected void sort()
  {
    Collections.sort( this.data );
  }
}
