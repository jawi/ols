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
 * Provides an annotation for one channel.
 */
public final class ChannelAnnotation
{
  // VARIABLES

  private final long startIndex;
  private final long endIndex;

  private final Object data;

  // CONSTRUCTORS

  /**
   * Creates a new ChannelAnnotation instance.
   */
  public ChannelAnnotation( final long aStartIndex, final long aEndIndex, final Object aData )
  {
    this.startIndex = aStartIndex;
    this.endIndex = aEndIndex;
    this.data = aData;
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
    if ( ( aObject == null ) || !( aObject instanceof ChannelAnnotation ) )
    {
      return false;
    }

    final ChannelAnnotation other = ( ChannelAnnotation )aObject;
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
  public long getEndIndex()
  {
    return this.endIndex;
  }

  /**
   * @return the startIndex
   */
  public long getStartIndex()
  {
    return this.startIndex;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( this.data == null ) ? 0 : this.data.hashCode() );
    result = prime * result + ( int )( this.endIndex ^ ( this.endIndex >>> 32 ) );
    result = prime * result + ( int )( this.startIndex ^ ( this.startIndex >>> 32 ) );
    return result;
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
