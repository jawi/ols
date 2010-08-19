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
