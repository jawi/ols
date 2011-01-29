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
package nl.lxtreme.ols.util.swing.validation;


/**
 * Provides a simple number validator.
 */
public final class NumberValidator implements IValidator
{
  // CONSTANTS

  /** The default radix, denoting decimal numbers. */
  private static final int DECIMAL_RADIX = 10;

  // VARIABLES

  private final Class<? extends Number> type;
  private final int radix;

  // CONSTRUCTORS

  /**
   * Creates a new NumberValidator instance for integer decimal values.
   */
  public NumberValidator()
  {
    this( Integer.TYPE );
  }

  /**
   * Creates a new NumberValidator instance for numeric (decimal) values of the
   * given type.
   * 
   * @param aType
   *          the numeric type (Integer, Long, ...) to expect.
   */
  public NumberValidator( final Class<? extends Number> aType )
  {
    this( aType, DECIMAL_RADIX );
  }

  /**
   * Creates a new NumberValidator instance for numeric values of the given type
   * and radix.
   * 
   * @param aType
   *          the numeric type (Integer, Long, ...) to expect;
   * @param aRadix
   *          the radix of the values to expect (2, 8, 10, 16).
   * @throws IllegalArgumentException
   *           in case the given type was either a float or double <b>and</b>
   *           the radix is not 10.
   */
  public NumberValidator( final Class<? extends Number> aType, final int aRadix )
  {
    if ( ( aRadix != DECIMAL_RADIX )
        && ( Float.class.isAssignableFrom( aType ) || Double.class.isAssignableFrom( aType ) ) )
    {
      throw new IllegalArgumentException( "Floats or doubles can only be validated as decimals (radix 10)!" );
    }
    this.type = aType;
    this.radix = aRadix;
  }

  /**
   * Creates a new NumberValidator instance for integer values with a given
   * radix.
   * 
   * @param aRadix
   *          the radix of the values to expect (2, 8, 10, 16).
   */
  public NumberValidator( final int aRadix )
  {
    this( Integer.TYPE, DECIMAL_RADIX );
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.swing.validation.IValidator#validate(java.lang.Object)
   */
  @Override
  public boolean validate( final Object aValue )
  {
    if ( aValue == null )
    {
      // Empty value is permitted...
      return true;
    }

    final String inputText = ( String.valueOf( aValue ) ).trim();
    if ( inputText.isEmpty() )
    {
      // Nothing given, this is allowed...
      return true;
    }

    if ( parse( inputText ) != null )
    {
      // A valid number was typed!
      return true;
    }

    return false;
  }

  /**
   * Parses the given input to a number, if possible.
   * 
   * @param aInputText
   *          the text to parse as number, cannot be <code>null</code>.
   * @return the parsed number, can be <code>null</code> in case the given text
   *         could not be parsed.
   */
  @SuppressWarnings( "boxing" )
  protected final Number parse( final String aInputText )
  {
    try
    {
      final Number value;
      if ( Long.TYPE == this.type )
      {
        value = Long.parseLong( aInputText, this.radix );
      }
      else if ( Short.TYPE == this.type )
      {
        value = Short.parseShort( aInputText, this.radix );
      }
      else if ( Float.TYPE == this.type )
      {
        value = Float.parseFloat( aInputText );
      }
      else if ( Double.TYPE == this.type )
      {
        value = Double.parseDouble( aInputText );
      }
      else
      // if ( Integer.TYPE == this.type )
      {
        value = Integer.parseInt( aInputText, this.radix );
      }

      return value;
    }
    catch ( NumberFormatException exception )
    {
      // Ignore, we're not interested in the details on why the input isn't a
      // number...
      return null;
    }
  }
}
