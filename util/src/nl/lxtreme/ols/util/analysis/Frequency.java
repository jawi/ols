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
package nl.lxtreme.ols.util.analysis;


import java.util.*;
import java.util.concurrent.*;


/**
 * Provides a frequency distribution.
 */
public final class Frequency<TYPE extends Comparable<TYPE>>
{
  // VARIABLES

  private final ConcurrentNavigableMap<TYPE, Long> distribution;

  // CONSTRUCTORS

  /**
   * Creates a new Frequency instance.
   */
  public Frequency()
  {
    this.distribution = new ConcurrentSkipListMap<TYPE, Long>();
  }

  /**
   * Creates a new Frequency instance using the given comparator for sorting the
   * added values.
   * 
   * @param aComparator
   *          the comparator to use for the distribution map, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given comparator was <code>null</code>.
   */
  public Frequency( final Comparator<TYPE> aComparator )
  {
    if ( aComparator == null )
    {
      throw new IllegalArgumentException( "Comparator cannot be null!" );
    }
    this.distribution = new ConcurrentSkipListMap<TYPE, Long>( aComparator );
  }

  // METHODS

  /**
   * Adds a given value to the distribution map.
   * 
   * @param aValue
   *          the value to add, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given value was <code>null</code>.
   */
  public void addValue( final TYPE aValue )
  {
    if ( aValue == null )
    {
      throw new IllegalArgumentException( "Value cannot be null!" );
    }

    synchronized ( this.distribution )
    {
      Long count = this.distribution.get( aValue );
      if ( count == null )
      {
        count = Long.valueOf( 1L );
      }
      else
      {
        count = Long.valueOf( count.longValue() + 1L );
      }
      this.distribution.put( aValue, count );
    }
  }

  /**
   * Clears all values from this frequency distribution.
   */
  public void clear()
  {
    this.distribution.clear();
  }

  /**
   * Counts the number of occurrences of the given value.
   * 
   * @param aValue
   *          the value to count, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given value was <code>null</code>.
   */
  public long getCount( final TYPE aValue )
  {
    if ( aValue == null )
    {
      throw new IllegalArgumentException( "Value cannot be null!" );
    }

    long result = 0L;

    Long count = this.distribution.get( aValue );
    if ( count != null )
    {
      result = count.longValue();
    }

    return result;
  }

  /**
   * Returns the item with the highest count or rank.
   * 
   * @return the item with the highest rank, can be <code>null</code> if this
   *         frequency distribution is empty.
   */
  public TYPE getHighestRanked()
  {
    TYPE result = null;

    Long rank = null;

    for ( final Map.Entry<TYPE, Long> entry : this.distribution.entrySet() )
    {
      if ( ( rank == null ) || ( rank.compareTo( entry.getValue() ) < 0 ) )
      {
        rank = entry.getValue();
        result = entry.getKey();
      }
    }

    return result;
  }

  /**
   * Returns the item with the lowest count or rank.
   * 
   * @return the item with the lowest rank, can be <code>null</code> if this
   *         frequency distribution is empty.
   */
  public TYPE getLowestRanked()
  {
    TYPE result = null;

    Long rank = null;

    for ( final Map.Entry<TYPE, Long> entry : this.distribution.entrySet() )
    {
      if ( ( rank == null ) || ( rank.compareTo( entry.getValue() ) > 0 ) )
      {
        rank = entry.getValue();
        result = entry.getKey();
      }
    }

    return result;
  }

  /**
   * Returns the number of entries in this frequency distribution map.
   * 
   * @return a size, >= 0.
   */
  public long getTotalCount()
  {
    long totalCount = 0L;

    for ( final Long count : this.distribution.values() )
    {
      if ( count != null )
      {
        totalCount += count.longValue();
      }
    }

    return totalCount;
  }

  /**
   * Returns the number of unique values in this frequency distribution map.
   * 
   * @return a unique value count, >= 0.
   */
  public int getUniqueValueCount()
  {
    int size = 0;

    size = this.distribution.keySet().size();

    return size;
  }

  /**
   * Returns an iterator for the values in this frequency distribution map.
   * 
   * @return an iterator with all values, sorted in natural order.
   */
  public Iterable<TYPE> values()
  {
    final List<TYPE> result = new ArrayList<TYPE>();

    result.addAll( this.distribution.keySet() );

    Collections.sort( result );
    return result;
  }
}
