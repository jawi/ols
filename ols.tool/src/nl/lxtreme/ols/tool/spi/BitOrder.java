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
package nl.lxtreme.ols.tool.spi;


import nl.lxtreme.ols.tool.base.*;


/**
 * Denotes in which order bits are to be interpreted in a byte.
 */
public enum BitOrder
{
  // CONSTANTS

  /**
   * Denotes the least significant bit is to be read first and the most
   * significant bit last (assuming you read from left to right).
   */
  LSB_FIRST,
  /**
   * Denotes the most significant bit is to be read first and the least
   * significant bit last (assuming you read from left to right).
   */
  MSB_FIRST;

  // METHODS

  /**
   * Converts the given value into a desired bit order.
   * 
   * @param aValue
   *          the value to convert;
   * @param aBitCount
   *          the number of bits that are supposed to be in the given value;
   * @param aBitOrder
   *          the desired bit order.
   * @return the converted value.
   */
  public static int convertBitOrder( final int aValue, final int aBitCount, final BitOrder aBitOrder )
  {
    if ( ( aBitCount <= 0 ) || ( aBitCount > 32 ) )
    {
      throw new IllegalArgumentException( "Bit count cannot be zero, negative or beyond 32-bits!" );
    }
    // We already have the most significant bit first, convert only if the bit
    // order is LSB first...
    if ( aBitOrder == BitOrder.MSB_FIRST )
    {
      return ( aValue & NumberUtils.getBitMask( aBitCount ) );
    }

    return NumberUtils.reverseBits( aValue, aBitCount );
  }

}
