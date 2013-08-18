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
 * Provides an annotation for a single channel.
 */
@Deprecated
public final class ChannelAnnotation implements Comparable<ChannelAnnotation>
{
  // VARIABLES

  private final long startIndex;
  private final long endIndex;
  private final Object data;

  // CONSTRUCTORS

  /**
   * Creates a new ChannelAnnotation instance.
   * 
   * @param aStartIndex
   *          the sample index on which this annotation starts;
   * @param aEndIndex
   *          the end index on which this annotation ends;
   * @param aData
   *          the actual annotated data.
   */
  public ChannelAnnotation( final long aStartIndex, final long aEndIndex, final Object aData )
  {
    this.startIndex = aStartIndex;
    this.endIndex = aEndIndex;
    this.data = aData;
  }

  // METHODS

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final ChannelAnnotation aAnnotation )
  {
    int result = this.startIndex < aAnnotation.startIndex ? -1 : ( this.startIndex == aAnnotation.startIndex ? 0 : 1 );
    if ( result == 0 )
    {
      result = this.endIndex < aAnnotation.endIndex ? -1 : ( this.endIndex == aAnnotation.endIndex ? 0 : 1 );
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object obj )
  {
    if ( this == obj )
    {
      return true;
    }
    if ( obj == null )
    {
      return false;
    }
    if ( !( obj instanceof ChannelAnnotation ) )
    {
      return false;
    }
    ChannelAnnotation other = ( ChannelAnnotation )obj;
    if ( this.data == null )
    {
      if ( other.data != null )
      {
        return false;
      }
    }
    else if ( !this.data.equals( other.data ) )
    {
      return false;
    }
    if ( this.endIndex != other.endIndex )
    {
      return false;
    }
    if ( this.startIndex != other.startIndex )
    {
      return false;
    }
    return true;
  }

  /**
   * @return the data
   */
  public Object getData()
  {
    return this.data;
  }

  /**
   * @return the endIndex
   */
  public long getEndTimestamp()
  {
    return this.endIndex;
  }

  /**
   * @return the startIndex
   */
  public long getStartTimestamp()
  {
    return this.startIndex;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( ( this.data == null ) ? 0 : this.data.hashCode() );
    result = ( prime * result ) + ( int )( this.endIndex ^ ( this.endIndex >>> 32 ) );
    result = ( prime * result ) + ( int )( this.startIndex ^ ( this.startIndex >>> 32 ) );
    return result;
  }

  /**
   * Returns whether this annotation starts before the given index and ends
   * after the given index.
   * 
   * @param aIndex
   *          the index.
   * @return <code>true</code> if this annotation is valid at the given index
   *         (see above), <code>false</code> otherwise.
   */
  public boolean isInRange( final long aTimestamp )
  {
    return ( getStartTimestamp() <= aTimestamp ) && ( getEndTimestamp() >= aTimestamp );
  }

  /**
   * Returns whether this annotation's start index is before the given end index
   * and its end index is after the given start index.
   * 
   * @param aStartIndex
   *          the starting index;
   * @param aEndIndex
   *          the ending index.
   * @return <code>true</code> if this annotation is valid between the given
   *         range (see above), <code>false</code> otherwise.
   */
  public boolean isInRange( final long aStartIndex, final long aEndIndex )
  {
    return ( getStartTimestamp() <= aEndIndex ) && ( getEndTimestamp() >= aStartIndex );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return this.data == null ? "" : String.valueOf( this.data );
  }
}
