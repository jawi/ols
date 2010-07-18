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
package nl.lxtreme.ols.tool.uart;


import java.util.*;


/**
 * Inner class for statistical baudrate analysis
 */
final class BaudRateAnalyzer
{
  // VARIABLES

  /*
   * Store as linked list with 2 int sized arrays as elements.
   * Each array element stores at index 0 the bitlength and at
   * index 1 the number of occurrences.
   */
  private final LinkedList<int[]> statData;

  // CONSTRUCTORS

  /*
   * create a histogram that allows to evaluate each
   * detected bitlength. The bitlength with the highest
   * occurrence is used for baudrate calculation.
   */
  public BaudRateAnalyzer( final int[] data, final long[] time, final int mask )
  {
    int a, b, c;
    int[] valuePair;
    long last = 0;
    b = data[0] & mask;
    a = 0;
    this.statData = new LinkedList<int[]>();
    for ( int i = 0; i < data.length; i++ )
    {
      if ( b != ( data[i] & mask ) )
      {
        a = ( int )( time[i] - last );
        c = findValue( a );
        if ( c < 0 )
        {
          valuePair = new int[2];
          valuePair[0] = a; // bitlength
          valuePair[1] = 1; // count
          this.statData.add( valuePair );
        }
        else
        {
          this.statData.get( c )[1]++;
        }
        last = time[i];
      }
      b = data[i] & mask;
    }
  }

  // METHODS

  /**
   * @return
   */
  public int getBest()
  {
    int rank = 0;
    int index = 0;
    for ( int i = 0; i < this.statData.size(); i++ )
    {
      if ( this.statData.get( i )[1] > rank )
      {
        rank = this.statData.get( i )[1];
        index = i;
      }
    }
    if ( this.statData.size() == 0 )
    {
      return 0;
    }
    return this.statData.get( index )[0];
  }

  /**
   * @return
   */
  public int getMax()
  {
    int max = 0;
    for ( int i = 0; i < this.statData.size(); i++ )
    {
      if ( this.statData.get( i )[0] > max )
      {
        max = this.statData.get( i )[0];
      }
    }
    return max;
  }

  /**
   * @return
   */
  public int getMin()
  {
    int min = Integer.MAX_VALUE;
    for ( int i = 0; i < this.statData.size(); i++ )
    {
      if ( this.statData.get( i )[0] < min )
      {
        min = this.statData.get( i )[0];
      }
    }
    return min;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return new String( "BaudRateAnalyzer:min=" + getMin() + ":max=" + getMax() + ":best=" + getBest() );
  }

  /**
   * @param val
   * @return
   */
  private int findValue( final int val )
  {
    for ( int i = 0; i < this.statData.size(); i++ )
    {
      if ( this.statData.get( i )[0] == val )
      {
        return i;
      }
    }
    return -1;
  }
}
