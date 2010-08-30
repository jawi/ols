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
package nl.lxtreme.ols.util;


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
   * Converts the bits of a byte into a desired order.
   * 
   * @param aValue
   *          the byte value to convert;
   * @param aBitOrder
   *          the bit order of the given value.
   * @return the converted value, always most significant byte first (assuming
   *         you are reading from left to right).
   */
  public static int convertByteOrder( final int aValue, final BitOrder aBitOrder )
  {
    int v = aValue;
    if ( aBitOrder == BitOrder.LSB_FIRST )
    {
      // Reverse the bits in a byte with 3 operations (64-bit multiply and
      // modulus division), taken from:
      // http://graphics.stanford.edu/~seander/bithacks.html#ReverseByteWith64BitsDiv
      v = ( int )( ( v * 0x0202020202L & 0x010884422010L ) % 1023 );
    }
    return v;
  }

  /**
   * Converts the bits of a long-word (32-bit) into a desired order.
   * 
   * @param aValue
   *          the 32-bit value to convert;
   * @param aBitOrder
   *          the bit order of the given value.
   * @return the converted 32-bit value, always most significant byte first
   *         (assuming you are reading from left to right).
   */
  public static int convertLongWordOrder( final int aValue, final BitOrder aBitOrder )
  {
    // Taken from:
    // http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
    long v = aValue;
    if ( aBitOrder == BitOrder.LSB_FIRST )
    {
      // swap odd and even bits
      v = ( ( v >> 1 ) & 0x55555555L ) | ( ( v & 0x55555555L ) << 1 );
      // swap consecutive pairs
      v = ( ( v >> 2 ) & 0x33333333L ) | ( ( v & 0x33333333L ) << 2 );
      // swap nibbles ...
      v = ( ( v >> 4 ) & 0x0F0F0F0FL ) | ( ( v & 0x0F0F0F0FL ) << 4 );
      // swap bytes
      v = ( ( v >> 8 ) & 0x00FF00FFL ) | ( ( v & 0x00FF00FFL ) << 8 );
      // swap 2-byte long pairs
      v = ( v >> 16 ) | ( v << 16 );
    }
    return ( int )( v );
  }

  /**
   * Converts the bits of a word value into a desired order.
   * 
   * @param aValue
   *          the 16-bit value to convert;
   * @param aBitOrder
   *          the bit order of the given value.
   * @return the converted 16-bit value, always most significant byte first
   *         (assuming you are reading from left to right).
   */
  public static int convertWordOrder( final int aValue, final BitOrder aBitOrder )
  {
    // Taken from:
    // http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
    long v = aValue;
    if ( aBitOrder == BitOrder.LSB_FIRST )
    {
      // swap odd and even bits
      v = ( ( v >> 1 ) & 0x55555555L ) | ( ( v & 0x55555555L ) << 1 );
      // swap consecutive pairs
      v = ( ( v >> 2 ) & 0x33333333L ) | ( ( v & 0x33333333L ) << 2 );
      // swap nibbles ...
      v = ( ( v >> 4 ) & 0x0F0F0F0FL ) | ( ( v & 0x0F0F0F0FL ) << 4 );
      // swap bytes
      v = ( ( v >> 8 ) & 0x00FF00FFL ) | ( ( v & 0x00FF00FFL ) << 8 );
    }
    return ( int )( v & 65535 );
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

      int result = Integer.valueOf( number );
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
