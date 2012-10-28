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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.base;


/**
 * 
 */
public class NumberUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link NumberUtilsTest} instance.
   */
  private NumberUtils()
  {
    // Nop
  }

  // METHODS

  /**
   * Returns the maximum value for the given bit count, e.g.,
   * <tt>( 1 << aBitCount ) - 1</tt>.
   * 
   * @param aBitCount
   *          the number of bits to create a bit mask for, > 0.
   * @return the bit mask.
   */
  public static int getBitMask( final int aBitCount )
  {
    if ( ( aBitCount <= 0 ) || ( aBitCount > 32 ) )
    {
      throw new IllegalArgumentException( "Invalid bit count, should be > 0 && <= 32." );
    }

    return ( int )( ( 1L << aBitCount ) - 1 );
  }

  /**
   * Calculates the percentage for the given value in the given range.
   * 
   * @param aValue
   *          the value;
   * @param aRange
   *          the range (zero-based).
   * @return the percentage (= value * 100.0 / aRange).
   */
  public static int getPercentage( final int aValue, final int aRange )
  {
    double value = 0.0;
    if ( aRange != 0 )
    {
      value = ( aValue * 100.0 ) / aRange;
    }

    return ( int )Math.max( 0.0, Math.min( 100.0, value ) );
  }

  /**
   * Calculates the percentage for the given value in the range denoted by the
   * given lower and upper bounds.
   * 
   * @param aValue
   *          the value;
   * @param aLowerBound
   *          the lower bound of the range;
   * @param aUpperBound
   *          the upper bound of the range.
   * @return the percentage (= value * 100.0 / range).
   */
  public static int getPercentage( final int aValue, final int aLowerBound, final int aUpperBound )
  {
    int range;
    int value = aValue;
    if ( aLowerBound > aUpperBound )
    {
      range = aLowerBound - aUpperBound;
      value = Math.max( 0, value - aUpperBound );
    }
    else
    {
      range = aUpperBound - aLowerBound;
      value = Math.max( 0, value - aLowerBound );
    }
    return getPercentage( value, range );
  }

  /**
   * Calculates the percentage for the given value in the given range.
   * 
   * @param aValue
   *          the value;
   * @param aRange
   *          the range (zero-based).
   * @return the percentage (= value * 100.0 / aRange).
   */
  public static int getPercentage( final long aValue, final long aRange )
  {
    double value = 0.0;
    if ( aRange != 0 )
    {
      value = ( aValue * 100.0 ) / aRange;
    }

    return ( int )Math.max( 0.0, Math.min( 100.0, value ) );
  }

  /**
   * Calculates the percentage for the given value in the range denoted by the
   * given lower and upper bounds.
   * 
   * @param aValue
   *          the value;
   * @param aLowerBound
   *          the lower bound of the range;
   * @param aUpperBound
   *          the upper bound of the range.
   * @return the percentage (= value * 100.0 / range).
   */
  public static int getPercentage( final long aValue, final long aLowerBound, final long aUpperBound )
  {
    long range;
    long value = aValue;
    if ( aLowerBound > aUpperBound )
    {
      range = aLowerBound - aUpperBound;
      value = Math.max( 0, value - aUpperBound );
    }
    else
    {
      range = aUpperBound - aLowerBound;
      value = Math.max( 0, value - aLowerBound );
    }
    return getPercentage( value, range );
  }

  /**
   * converts an integer to a bin string with leading zeros
   * 
   * @param aValue
   *          integer value for conversion
   * @param aFieldWidth
   *          number of charakters in field
   * @return a nice string
   */
  public static String integerToBinString( final int aValue, final int aFieldWidth )
  {
    // first build a mask to cut off the signed extension
    final long mask = ( long )( Math.pow( 2.0, aFieldWidth ) - 1L );

    StringBuilder sb = new StringBuilder( Long.toBinaryString( aValue & mask ) );

    int numberOfLeadingZeros = Math.max( 0, aFieldWidth - sb.length() );
    for ( ; numberOfLeadingZeros > 0; numberOfLeadingZeros-- )
    {
      sb.insert( 0, '0' );
    }

    return sb.toString();
  }

  /**
   * converts an integer to a hex string with leading zeros
   * 
   * @param aValue
   *          integer value for conversion
   * @param aFieldWidth
   *          number of charakters in field
   * @return a nice string
   */
  public static String integerToHexString( final int aValue, final int aFieldWidth )
  {
    // first build a mask to cut off the signed extension
    final long mask = ( long )( Math.pow( 16.0, aFieldWidth ) - 1L );

    StringBuilder sb = new StringBuilder( Long.toHexString( aValue & mask ) );

    int numberOfLeadingZeros = Math.max( 0, aFieldWidth - sb.length() );
    for ( ; numberOfLeadingZeros > 0; numberOfLeadingZeros-- )
    {
      sb.insert( 0, '0' );
    }

    return sb.toString();
  }

  /**
   * Returns whether the given value is a power of two.
   * 
   * @param aValue
   *          the value to test as power of two.
   * @return <code>true</code> if the given value is a power of two,
   *         <code>false</code> otherwise.
   */
  public static boolean isPowerOfTwo( final int aValue )
  {
    // See:
    // <http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2>
    return ( aValue != 0 ) && ( ( aValue & ( aValue - 1 ) ) == 0 );
  }

  /**
   * Converts the bits of a byte into a desired order.
   * 
   * @param aValue
   *          the byte value to convert.
   * @return the converted value, always most significant byte first (assuming
   *         you are reading from left to right).
   */
  public static int reverseBits( final int aValue, final int aBitCount )
  {
    if ( isPowerOfTwo( aBitCount ) )
    {
      // Taken from:
      // http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
      long v = aValue;

      int s = aBitCount;
      int mask = getBitMask( aBitCount );
      while ( ( s >>= 1 ) > 0 )
      {
        mask ^= ( mask << s );
        v = ( ( v >> s ) & mask ) | ( ( v << s ) & ~mask );
      }

      return ( int )( v );
    }
    else
    {
      int r = 0;
      int v = aValue;
      int s = aBitCount;

      for ( ; v != 0; v >>= 1 )
      {
        r <<= 1;
        r |= ( v & 1 );
        s--;
      }

      if ( s >= 0 )
      {
        r <<= s; // shift when v's highest bits are zero
      }

      return r;
    }
  }

}
