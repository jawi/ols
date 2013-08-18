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
package nl.lxtreme.ols.util;


import java.io.*;
import java.nio.*;
import java.util.*;
import java.util.regex.*;


/**
 * Provides some utility methods for parsing numbers, etc.
 */
public final class NumberUtils
{
  // ENUMERATIONS

  /**
   * Denotes in which order bits are to be interpreted in a byte.
   */
  public enum BitOrder
  {
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
  }

  /**
   * Denotes how "k", "M" units should be interpreted.
   */
  public enum UnitDefinition
  {
    SI, BINARY;
  }

  /**
   * Provides a numeric comparator, sorts in decrementing order.
   * <p>
   * Yes, this is quite a hacky approach, but unfortunately, it is currently the
   * only way instead of writing 6 almost equivalent classes.
   * </p>
   */
  private static class NumericComparator<N extends Number> implements Serializable, Comparator<N>
  {
    private static final long serialVersionUID = 1L;

    private final boolean sortAscending;

    /**
     * Creates a new NumberUtils.NumericComparator instance.
     */
    NumericComparator( final boolean aSortAscending )
    {
      this.sortAscending = aSortAscending;
    }

    /**
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    @SuppressWarnings( { "rawtypes", "unchecked" } )
    public int compare( final N aO1, final N aO2 )
    {
      if ( aO1 instanceof Comparable<?> )
      {
        return this.sortAscending ? //
        ( ( Comparable )aO1 ).compareTo( aO2 )
            : //
            ( ( Comparable )aO2 ).compareTo( aO1 );
      }
      else
      {
        throw new IllegalArgumentException( "Your number instance should implement Comparable!" );
      }
    }
  }

  // CONSTANTS

  private static final Pattern SMART_INT_PATTERN = Pattern.compile( "^([-+]?\\d+)(?:\\s*([kKM]))?.*$" );

  // CONSTRUCTORS

  /**
   * Creates a new NumberUtils instance, never used.
   */
  private NumberUtils()
  {
    // NO-op
  }

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
      return ( aValue & getBitMask( aBitCount ) );
    }

    return reverseBits( aValue, aBitCount );
  }

  /**
   * Converts the given value into a desired byte order.
   * 
   * @param aValue
   *          the value to convert;
   * @param aByteCount
   *          the number of bytes that are supposed to be in the given value;
   * @param aByteOrder
   *          the desired byte order.
   * @return the converted value.
   */
  public static int convertByteOrder( final int aValue, final int aByteCount, final ByteOrder aByteOrder )
  {
    if ( ( aByteCount <= 0 ) || ( aByteCount > 32 ) )
    {
      throw new IllegalArgumentException( "Bit count cannot be zero, negative or beyond 32-bits!" );
    }

    final ByteBuffer buf = ByteBuffer.allocate( aByteCount );
    buf.putInt( aValue );
    buf.order( aByteOrder );
    buf.position( 0 );
    final int result = buf.getInt();
    return result;
  }

  /**
   * Creates a number comparator that can be used to sort a list of numbers in
   * ascending or descending order.
   * 
   * @param <T>
   *          the exact numeric type to sort on;
   * @param aSortAscending
   *          <code>true</code> if the numbers should be sorted ascendingly,
   *          <code>false</code> if the numbers should be sorted in descending
   *          order.
   * @return a comparator instance, never <code>null</code>.
   */
  public static <T extends Number> Comparator<T> createNumberComparator( final boolean aSortAscending )
  {
    return new NumericComparator<T>( aSortAscending );
  }

  /**
   * Returns the largest bit-value that is set to '1' of a given mask value.
   * <p>
   * E.g., for a mask value of 128, the result will be 7, while for a mask value
   * of 3, the value will be 1.
   * </p>
   * 
   * @param aMaskValue
   *          the mask value to return the bit-index of.
   * @return the largest bit-value that is set to '1' of a given mask value,
   *         zero-based.
   */
  public static int getBitIndex( final int aMaskValue )
  {
    return ( int )Math.floor( Math.log( aMaskValue ) / Math.log( 2 ) );
  }

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
      value = aValue * 100.0 / aRange;
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
      value = aValue * 100.0 / aRange;
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
   * "Packs" a given value according to the given byte mask by removing all zero
   * bytes from the given integer value and shifting all non-zero bytes to
   * consecutive places.
   * <p>
   * For example: given <tt>aValue</tt> == 0x12003400, the result would be
   * 0x00001234.
   * </p>
   * 
   * @param aValue
   *          the 4-byte (32-bit) value to pack.
   * @return the packed representation of the given value.
   */
  public static int packBytes( final int aValue )
  {
    int mask = 0xFF000000;
    int result = 0;
    for ( int i = 0, j = 3; i < 4; i++, j-- )
    {
      int tmp = ( aValue & mask ) >> ( j * 8 );
      if ( tmp != 0 )
      {
        result <<= 8;
        result |= tmp;
      }
      mask >>>= 8;
    }
    return result;
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

  /**
   * Parses the given text as an integer, avoiding runtime exceptions.
   * 
   * @param aText
   *          the text to parse as an integer, can be <code>null</code> or
   *          empty.
   * @return the numeric representation of the given text, or -1 if the text
   *         could not be parsed correctly as integer.
   * @see #safeParseInt(String, int)
   */
  public static int safeParseInt( final String aText )
  {
    return safeParseInt( aText, -1 );
  }

  /**
   * Parses the given text as an integer, avoiding runtime exceptions.
   * 
   * @param aText
   *          the text to parse as an integer, can be <code>null</code> or
   *          empty;
   * @param aDefault
   *          the default value to return in case parsing failed.
   * @return the numeric representation of the given text, or the given default
   *         if the text could not be parsed correctly as integer.
   * @see Integer#parseInt(String)
   */
  public static int safeParseInt( final String aText, final int aDefault )
  {
    try
    {
      return Integer.parseInt( aText );
    }
    catch ( NumberFormatException exception )
    {
      return aDefault;
    }
  }

  /**
   * Parses the given text as an integer, avoiding runtime exceptions.
   * 
   * @param aText
   *          the text to parse as an integer, can be <code>null</code> or
   *          empty.
   * @return the numeric representation of the given text, or -1 if the text
   *         could not be parsed correctly as integer.
   * @see #safeParseLong(String, long)
   */
  public static long safeParseLong( final String aText )
  {
    return safeParseLong( aText, -1L );
  }

  /**
   * Parses the given text as an integer, avoiding runtime exceptions.
   * 
   * @param aText
   *          the text to parse as an integer, can be <code>null</code> or
   *          empty;
   * @param aDefault
   *          the default value to return in case parsing failed.
   * @return the numeric representation of the given text, or the given default
   *         if the text could not be parsed correctly as integer.
   * @see Long#parseLong(String)
   */
  public static long safeParseLong( final String aText, final long aDefault )
  {
    try
    {
      return Long.parseLong( aText );
    }
    catch ( NumberFormatException exception )
    {
      return aDefault;
    }
  }

  /**
   * Provides a "smart" integer parsing routine that allows (decimal) numbers in
   * string form with all kind of trailing characters to be parsed into an
   * integer. Some trailing characters are understood as being part of the
   * number, like "k" to denote a value in thousands, or "m" to denote a value
   * in millions.
   * <p>
   * Characters recognized are: "k" and "M" to denote units of 1024, 1024*1024.
   * </p>
   * 
   * @param aText
   *          the text to parse into an integer value, cannot be
   *          <code>null</code>.
   * @return the integer value part of the given text, or 0 if the text couldn't
   *         be parsed.
   */
  public static int smartParseInt( final String aText )
  {
    return smartParseInt( aText, 0 );
  }

  /**
   * Provides a "smart" integer parsing routine that allows (decimal) numbers in
   * string form with all kind of trailing characters to be parsed into an
   * integer. Some trailing characters are understood as being part of the
   * number, like "k" to denote a value in thousands, or "m" to denote a value
   * in millions.
   * <p>
   * Characters recognized are: "k" and "M" to denote units of 1024, 1024*1024.
   * </p>
   * 
   * @param aText
   *          the text to parse into an integer value, cannot be
   *          <code>null</code>;
   * @param aDefault
   *          the default value to return in case the given text couldn't be
   *          parsed into a valid number.
   * @return the integer value part of the given text, or the given default
   *         value if the text couldn't be parsed.
   */
  public static int smartParseInt( final String aText, final int aDefault )
  {
    return smartParseInt( aText, UnitDefinition.BINARY, aDefault );
  }

  /**
   * Provides a "smart" integer parsing routine that allows (decimal) numbers in
   * string form with all kind of trailing characters to be parsed into an
   * integer. Some trailing characters are understood as being part of the
   * number, like "k" to denote a value in thousands, or "m" to denote a value
   * in millions.
   * 
   * @param aText
   *          the text to parse into an integer value, cannot be
   *          <code>null</code>;
   * @param aUnitDefinition
   *          the unit definition for "k" and "M" characters, should be either
   *          SI (units of 1000) or BINARY (units of 1024).
   * @return the integer value part of the given text, or 0 if the text couldn't
   *         be parsed.
   */
  public static int smartParseInt( final String aText, final UnitDefinition aUnitDefinition )
  {
    return smartParseInt( aText, aUnitDefinition, 0 );
  }

  /**
   * Provides a "smart" integer parsing routine that allows (decimal) numbers in
   * string form with all kind of trailing characters to be parsed into an
   * integer. Some trailing characters are understood as being part of the
   * number, like "k" to denote a value in thousands, or "m" to denote a value
   * in millions.
   * 
   * @param aText
   *          the text to parse into an integer value, cannot be
   *          <code>null</code>;
   * @param aUnitDefinition
   *          the unit definition for "k" and "M" characters, should be either
   *          SI (units of 1000) or BINARY (units of 1024);
   * @param aDefault
   *          the default value to return in case the given text couldn't be
   *          parsed into a valid number.
   * @return the integer value part of the given text, or the given default
   *         value if the text couldn't be parsed.
   */
  public static int smartParseInt( final String aText, final UnitDefinition aUnitDefinition, final int aDefault )
  {
    // Avoid NPEs when given a null argument; also when an empty
    // string is given, we can be fairly quick in our conclusion...
    if ( ( aText == null ) || aText.trim().isEmpty() )
    {
      return aDefault;
    }

    final Matcher matcher = SMART_INT_PATTERN.matcher( aText );
    if ( matcher.matches() )
    {
      final String number = matcher.group( 1 );
      final String unit = matcher.group( 2 );

      int result = Integer.parseInt( number );
      if ( unit != null )
      {
        result *= parseUnit( unit, aUnitDefinition );
      }
      return result;
    }
    return aDefault;
  }

  /**
   * Parses a given unit-character using the given unit-definition.
   * 
   * @param aUnit
   *          the unit character (k, M, G) to parse;
   * @param aUnitDefinition
   *          the definition of the unit characters (units of 1000 or 1024).
   * @return a multiplier for the given unit-character, defaults to 1.
   */
  private static long parseUnit( final String aUnit, final UnitDefinition aUnitDefinition )
  {
    if ( "k".equalsIgnoreCase( aUnit ) )
    {
      return ( UnitDefinition.SI == aUnitDefinition ) ? 1000L : 1024L;
    }
    else if ( "m".equalsIgnoreCase( aUnit ) )
    {
      return ( UnitDefinition.SI == aUnitDefinition ) ? 1000000L : 1048576L;
    }
    else if ( "g".equalsIgnoreCase( aUnit ) )
    {
      return ( UnitDefinition.SI == aUnitDefinition ) ? 1000000000L : 1073741824L;
    }
    return 1L;
  }
}
