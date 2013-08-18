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


/**
 * Denotes a rising/falling signal edge.
 */
public enum Edge
{
  /** No edge. */
  NONE, //
  /** A rising edge (low -> high transition). */
  RISING, //
  /** A falling edge (high -> low transition). */
  FALLING;

  /**
   * Given two (sample) values, determines whether they form a falling, rising
   * or no edge.
   * <p>
   * If the given "old" value is strictly greater than the given "new" value,
   * this will be considered a falling edge. If the "old" value is strictly less
   * than the "new" value, this will be considered a rising edge. If both values
   * are equal, this means no edge.
   * </p>
   * 
   * @param aOldValue
   *          the "old" value;
   * @param aNewValue
   *          the "new" value.
   * @return the edge corresponding to the given values, never <code>null</code>
   *         .
   */
  public static Edge toEdge( final int aOldValue, final int aNewValue )
  {
    if ( aOldValue > aNewValue )
    {
      return FALLING;
    }
    else if ( aOldValue < aNewValue )
    {
      return RISING;
    }
    return NONE;
  }

  /**
   * Returns the inverse of this edge, so return 'falling' it this is a 'rising'
   * edge, and the other way around. In case this edge is 'none', 'none' will be
   * returned.
   * 
   * @return the inverse of this edge, never <code>null</code>.
   */
  public Edge invert()
  {
    if ( isRising() )
    {
      return FALLING;
    }
    else if ( isFalling() )
    {
      return RISING;
    }
    return NONE;
  }

  /**
   * Returns whether this is a falling edge or not.
   * 
   * @return <code>true</code> if this is a falling edge, <code>false</code>
   *         otherwise.
   */
  public boolean isFalling()
  {
    return this == FALLING;
  }

  /**
   * Returns whether this is not an edge at all.
   * 
   * @return <code>true</code> if this is "no" edge, <code>false</code>
   *         otherwise.
   */
  public boolean isNone()
  {
    return this == NONE;
  }

  /**
   * Returns whether this is a rising edge or not.
   * 
   * @return <code>true</code> if this is a rising edge, <code>false</code>
   *         otherwise.
   */
  public boolean isRising()
  {
    return this == RISING;
  }
}
