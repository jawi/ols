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


/**
 * 
 */
public final class DisplayUtils
{
  // INNER TYPES

  // CONSTRUCTORS

  /**
   * Creates a new DisplayUtils instance (never used).
   */
  private DisplayUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Returns a percentage as string value.
   * 
   * @param aPercentage
   *          the percentage, >= 0.0 and <= 1.0.
   * @return a percentage display value, like "59.3%", neer <code>null</code>.
   */
  public static String displayPercentage( final double aPercentage )
  {
    return String.format( "%.1f%%", Double.valueOf( aPercentage * 100.0 ) );
  }

  /**
   * Returns the ordinal representation (in English) of the given value.
   * 
   * @param aValue
   *          the value to get the ordinal value for, >= 0 && < 40.
   * @return a ordinal number representation of the given value, like "1st".
   */
  public static String getOrdinalNumber( final int aValue )
  {
    String suffix = "";
    if ( ( aValue == 1 ) || ( aValue == 21 ) || ( aValue == 31 ) )
    {
      suffix = "st";
    }
    else if ( ( aValue == 2 ) || ( aValue == 22 ) )
    {
      suffix = "nd";
    }
    else if ( ( aValue == 3 ) || ( aValue == 23 ) )
    {
      suffix = "rd";
    }
    else if ( ( aValue >= 0 ) && ( aValue < 40 ) )
    {
      suffix = "th";
    }
    return String.format( "%d%s", Integer.valueOf( aValue ), suffix );
  }
}

/* EOF */
