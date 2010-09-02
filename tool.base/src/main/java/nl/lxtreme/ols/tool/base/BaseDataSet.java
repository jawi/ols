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
package nl.lxtreme.ols.tool.base;


import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a base data set implementation.
 */
public class BaseDataSet<DATA extends BaseData<DATA>>
{
  // VARIABLES

  private final List<DATA> data;
  private final long startOfDecode;
  private final long endOfDecode;
  private final boolean timingDataPresent;
  private final int sampleRate;

  // CONSTRUCTORS

  /**
   * Creates a new BaseDataSet.
   * 
   * @param aStartOfDecode
   * @param aEndOfDecode
   * @param aData
   */
  public BaseDataSet( final long aStartOfDecode, final long aEndOfDecode, final CapturedData aData )
  {
    this.data = new ArrayList<DATA>();

    this.startOfDecode = aStartOfDecode;
    this.endOfDecode = aEndOfDecode;

    this.timingDataPresent = aData.hasTimingData();
    this.sampleRate = aData.getSampleRate();
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
   * @return
   */
  public final long getEndOfDecode()
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
   * @return
   */
  public final long getStartOfDecode()
  {
    return this.startOfDecode;
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
   * Determines the time (display) value for a given sample index.
   * 
   * @param aIndex
   *          the sample index to convert to a time (display) value.
   * @return the time display value of the given sample index, never
   *         <code>null</code>.
   */
  protected final String indexToTime( final long aIndex )
  {
    final long index = ( aIndex - this.startOfDecode );
    if ( this.timingDataPresent )
    {
      return DisplayUtils.displayScaledTime( index, this.sampleRate );
    }
    else
    {
      return Long.toString( index );
    }
  }

  /**
   * Sorts the data according to the {@link Comparable} implementation of DATA.
   */
  protected void sort()
  {
    Collections.sort( this.data );
  }
}
