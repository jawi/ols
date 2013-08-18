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
package nl.lxtreme.ols.tool.onewire;


import nl.lxtreme.ols.api.data.*;


/**
 * Represents decoded 1-wire data.
 */
public class OneWireData extends BaseData<OneWireData>
{
  // VARIABLES

  private final int value;

  // CONSTRUCTORS

  /**
   * Creates a new OneWireData instance.
   * 
   * @param aIdx
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   * @param aValue
   */
  public OneWireData( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aValue )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.value = aValue;
  }

  /**
   * Creates a new OneWireData instance.
   * 
   * @param aIdx
   * @param aChannelIdx
   * @param aSampleIdx
   * @param aEventName
   */
  public OneWireData( final int aIdx, final int aChannelIdx, final int aSampleIdx, final String aEventName,
      final boolean aSlavePresent )
  {
    super( aIdx, aChannelIdx, aSampleIdx, aEventName );
    this.value = aSlavePresent ? 1 : 0;
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
    if ( !super.equals( aObject ) || !( aObject instanceof OneWireData ) )
    {
      return false;
    }

    final OneWireData other = ( OneWireData )aObject;
    if ( this.value != other.value )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the data value (if this is not an event).
   * 
   * @return the data value.
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
    result = ( prime * result ) + this.value;
    return result;
  }
}
