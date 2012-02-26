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
package nl.lxtreme.ols.api.util;


/**
 * Represents a size unit (SI-standard) that has a displayable representation
 * and has a scale factor.
 */
public enum SizeUnit
{
  // CONSTANTS

  B( "", 1.0 ), //
  KB( "k", 1024.0 ), //
  MB( "M", 1048576.0 ), //
  GB( "G", 1073741824.0 ), //
  TB( "T", 1099511627776.0 ); //

  // VARIABLES

  private String displayName;
  private double factor;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SizeUnit} instance.
   */
  private SizeUnit( final String aPrefix, final double aFactor )
  {
    this.displayName = aPrefix.concat( "B" );
    this.factor = aFactor;
  }

  // METHODS

  /**
   * Convenience method to convert a given size to a displayable representation.
   * 
   * @param aSize
   *          the size to convert to a displayable representation.
   * @return a displayable representation of the given size, never
   *         <code>null</code>.
   */
  public static String toString( final double aSize )
  {
    SizeUnit unit = valueOf( aSize );
    final Double s = Double.valueOf( aSize / unit.getFactor() );
    return String.format( "%.3f%s", s, unit.getDisplayName() );
  }

  /**
   * Returns a {@link SizeUnit} instance usable for representing the given size.
   * 
   * @param aSize
   *          the size to convert to a {@link SizeUnit} instance.
   * @return a {@link SizeUnit} instance, never <code>null</code>.
   */
  public static SizeUnit valueOf( final double aSize )
  {
    SizeUnit[] sizes = values();

    int i = sizes.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( aSize >= sizes[i].getFactor() )
      {
        break;
      }
    }

    return sizes[Math.max( i, 0 )];
  }

  /**
   * Returns the current value of displayName.
   * 
   * @return the displayName
   */
  public String getDisplayName()
  {
    return this.displayName;
  }

  /**
   * Returns the current value of factor.
   * 
   * @return the factor
   */
  public double getFactor()
  {
    return this.factor;
  }
}
