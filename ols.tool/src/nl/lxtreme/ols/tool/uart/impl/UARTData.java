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
package nl.lxtreme.ols.tool.uart.impl;


import nl.lxtreme.ols.api.data.*;


/**
 * Class for UART dataset
 * 
 * @author Frank Kunz
 */
public final class UARTData extends BaseData<UARTData>
{
  // CONSTANTS

  public static final int UART_TYPE_EVENT = 0;
  public static final int UART_TYPE_RXEVENT = 1;
  public static final int UART_TYPE_TXEVENT = 2;
  public static final int UART_TYPE_RXDATA = 3;
  public static final int UART_TYPE_TXDATA = 4;

  // VARIABLES

  private final int data;
  private final int type;

  // CONSTRUCTORS

  /*
   * data
   */
  public UARTData( final int aIndex, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aData, final int aType )
  {
    super( aIndex, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.data = aData;
    this.type = aType;
  }

  /*
   * generic event
   */
  public UARTData( final int aIndex, final int aChannelIdx, final int aSampleIdx, final String aEvent )
  {
    this( aIndex, aChannelIdx, aSampleIdx, aEvent, UART_TYPE_EVENT );
  }

  /*
   * type specific event
   */
  public UARTData( final int aIndex, final int aChannelIdx, final int aSampleIdx, final String aEvent, final int aType )
  {
    super( aIndex, aChannelIdx, aSampleIdx, aEvent );
    this.data = 0;
    this.type = aType;
  }

  // METHODS

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final UARTData aComparable )
  {
    return ( getStartSampleIndex() - aComparable.getStartSampleIndex() );
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
    if ( !super.equals( aObject ) || !( aObject instanceof UARTData ) )
    {
      return false;
    }

    final UARTData other = ( UARTData )aObject;
    if ( this.data != other.data )
    {
      return false;
    }
    if ( this.type != other.type )
    {
      return false;
    }
    return true;
  }

  /**
   * @return
   */
  public int getData()
  {
    return this.data;
  }

  /**
   * @return
   */
  public int getType()
  {
    return this.type;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + this.data;
    result = prime * result + this.type;
    return result;
  }

  /**
   * @return
   */
  @Override
  public boolean isEvent()
  {
    return ( this.type == UART_TYPE_EVENT ) || ( this.type == UART_TYPE_RXEVENT ) || ( this.type == UART_TYPE_TXEVENT );
  }
}
