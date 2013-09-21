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
package nl.lxtreme.ols.tool.i2c;


import nl.lxtreme.ols.api.data.*;


/**
 * Class for I2C dataset
 * 
 * @author Frank Kunz An I2C dataset consists of a timestamp, a value, or it can
 *         have an I2C event. This class is used to store the decoded I2C data
 *         in a Vector.
 */
public final class I2CData extends BaseData<I2CData>
{
  // VARIABLES

  private final int value;

  // CONSTRUCTORS

  /**
   * @param aTime
   * @param aValue
   */
  public I2CData( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aValue )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.value = aValue;
  }

  /**
   * @param aTime
   * @param aEventName
   */
  public I2CData( final int aIdx, final int aChannelIdx, final int aSampleIdx, final String aEventName )
  {
    super( aIdx, aChannelIdx, aSampleIdx, aEventName );
    this.value = 0;
  }

  // METHODS

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
    if ( !super.equals( aObject ) || !( aObject instanceof I2CData ) )
    {
      return false;
    }

    final I2CData other = ( I2CData )aObject;
    if ( this.value != other.value )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the value.
   * 
   * @return the value, never <code>null</code>.
   */
  public int getValue()
  {
    return this.value;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + this.value;
    return result;
  }
}
