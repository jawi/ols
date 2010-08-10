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


/**
 * Provides a base data implementation.
 */
public abstract class BaseData<TYPE extends Comparable<? super TYPE>> implements Comparable<TYPE>
{
  // VARIABLES

  private final long time;
  private final String timeDisplayValue;

  // CONSTRUCTORS

  /**
   * Creates a new BaseData instance.
   */
  public BaseData( final long aTime, final String aTimeDisplayValue )
  {
    this.time = aTime;
    this.timeDisplayValue = aTimeDisplayValue;
  }

  // METHODS

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( final TYPE aComparable )
  {
    return ( int )( this.time - ( ( BaseData<?> )aComparable ).getTime() );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( aObject == null )
    {
      return false;
    }
    if ( !( aObject instanceof BaseData<?> ) )
    {
      return false;
    }

    BaseData<?> other = ( BaseData<?> )aObject;
    if ( this.time != other.time )
    {
      return false;
    }

    return true;
  }

  /**
   * @return the time
   */
  public final long getTime()
  {
    return this.time;
  }

  /**
   * @return the timeDisplayValue
   */
  public final String getTimeDisplayValue()
  {
    return this.timeDisplayValue;
  };

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( int )( this.time ^ ( this.time >>> 32 ) );
    return result;
  }
}
