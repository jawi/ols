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
 * Denotes a sample of data, which has a value and a timestamp.
 */
@Deprecated
public final class Sample implements Cloneable
{
  // CONSTANTS

  private static final int DEFAULT_SAMPLE_WIDTH = 4;

  // VARIABLES

  private long timestamp;
  private int value;
  private int width;

  // CONSTRUCTORS

  /**
   * Creates a new 32-bit Sample instance.
   * 
   * @param aTimestamp
   *          the timestamp of this sample;
   * @param aValue
   *          the sample value.
   */
  public Sample( final long aTimestamp, final int aValue )
  {
    this( aTimestamp, aValue, DEFAULT_SAMPLE_WIDTH );
  }

  /**
   * Creates a new 32-bit Sample instance.
   * 
   * @param aTimestamp
   *          the timestamp of this sample;
   * @param aValue
   *          the sample value;
   * @param aWidth
   *          the width of this sample, in bytes.
   */
  public Sample( final long aTimestamp, final int aValue, final int aWidth )
  {
    this.timestamp = aTimestamp;
    this.value = aValue;
    this.width = aWidth;
  }

  // METHODS

  /**
   * Creates a copy of this sample.
   * 
   * @return a new copy of this sample, never <code>null</code>.
   * @see java.lang.Object#clone()
   */
  @Override
  public Sample clone() throws CloneNotSupportedException
  {
    final Sample result = ( Sample )super.clone();
    result.timestamp = this.timestamp;
    result.value = this.value;
    result.width = this.width;
    return result;
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
    if ( ( aObject == null ) || !( aObject instanceof Sample ) )
    {
      return false;
    }

    final Sample other = ( Sample )aObject;
    if ( this.timestamp != other.timestamp )
    {
      return false;
    }

    if ( this.value != other.value )
    {
      return false;
    }

    if ( this.width != other.width )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the timestamp.
   * 
   * @return the timestamp, as long value.
   */
  public long getTimestamp()
  {
    return this.timestamp;
  }

  /**
   * Returns the sample value.
   * 
   * @return the value, as 32-bit integer.
   */
  public int getValue()
  {
    return this.value;
  }

  /**
   * Returns the width of this sample.
   * 
   * @return the width, in bytes.
   */
  public int getWidth()
  {
    return this.width;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( int )( this.timestamp ^ ( this.timestamp >>> 32 ) );
    result = ( prime * result ) + this.value;
    result = ( prime * result ) + this.width;
    return result;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return Integer.toHexString( getValue() ) + "@" + Long.toString( getTimestamp() );
  }
}
