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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client;


import java.util.*;


/**
 * Represent various radices that can be used to display data in.
 */
public enum Radix
{
  // CONSTANTS

  /** Hexadecimal radix (base 16). */
  HEX( "Hexadecimal", 16 ),
  /** Decimal radix (base 10). */
  DEC( "Decimal", 10 ),
  /** Octal radix (base 8). */
  OCT( "Octal", 8 ),
  /** Binary radix (base 2). */
  BIN( "Binary", 2 );

  // VARIABLES

  private final String name;
  private final int base;
  private final double width;

  // CONSTRUCTORS

  /**
   * Creates a new {@link Radix} instance.
   */
  private Radix( final String aName, final int aBase )
  {
    this.name = aName;
    this.base = aBase;
    this.width = Math.log10( aBase ) / Math.log10( 2 );
  }

  // METHODS

  /**
   * @return the radix base, > 0.
   */
  public int getBase()
  {
    return this.base;
  }

  /**
   * @return a display name for this radix.
   */
  public String getDisplayName()
  {
    return this.name;
  }

  /**
   * @return the number of bits needed to represent a single digit, &gt; 0.
   */
  public double getWidth()
  {
    return this.width;
  }

  /**
   * Returns a string representation of the given value prepending the result
   * with '0's until the given width is met.
   *
   * @param aValue
   *          the value to convert to a string representation.
   * @return the string representation of the given value in this radix.
   */
  public String toString( final int aValue )
  {
    return Integer.toString( aValue, this.base ).toUpperCase( Locale.ENGLISH );
  }

  /**
   * Returns a string representation of the given value prepending the result
   * with '0's until the given length is met.
   *
   * @param aValue
   *          the value to convert to a string representation;
   * @param aLength
   *          the total width of the returned string, &gt; 0.
   * @return the string representation of the given value in this radix.
   */
  public String toString( final int aValue, final int aLength )
  {
    StringBuilder text = new StringBuilder( aLength );
    text.append( toString( aValue ) );
    while ( text.length() < aLength )
    {
      text.insert( 0, '0' );
    }
    return text.toString();
  }

  @Override
  public String toString()
  {
    return this.name;
  }
}
