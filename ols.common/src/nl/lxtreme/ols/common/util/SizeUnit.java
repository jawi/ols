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
package nl.lxtreme.ols.common.util;


/**
 * Represents a size unit (SI-standard) that has a displayable representation
 * and has a scale factor.
 */
public enum SizeUnit
{
  // CONSTANTS

  /** bytes. */
  B( "", 1.0 ),
  /** kilobytes. */
  KB( "k", 1024.0 ),
  /** megabytes. */
  MB( "M", 1048576.0 ),
  /** gigabytes. */
  GB( "G", 1073741824.0 ),
  /** terabytes. */
  TB( "T", 1099511627776.0 );

  /** Constant used to determine whether we should show "0B". */
  public static final double ZERO_THRESHOLD = 1.0e-1;
  /** All units are in bytes. */
  private static final String BASE_UNIT = "B";

  // VARIABLES

  private final String displayName;
  private final double factor;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SizeUnit} instance.
   */
  private SizeUnit( final String aPrefix, final double aFactor )
  {
    this.displayName = aPrefix.concat( BASE_UNIT );
    this.factor = aFactor;
  }

  // METHODS

  /**
   * Convenience method to directly get a displayable represention of a given
   * size.
   * <p>
   * This method does the same as calling:
   * <code>toUnit( aSize ).format( aSize, 2 );</code>.
   * </p>
   * 
   * @param aSize
   *          the size to get a displayable representation for.
   * @return a string representation of the given size, never <code>null</code>.
   */
  public static String format( final double aSize )
  {
    return toUnit( aSize ).format( aSize, 2 );
  }

  /**
   * Returns a {@link SizeUnit} instance usable for representing the given size.
   * 
   * @param aSize
   *          the size to convert to a {@link SizeUnit} instance.
   * @return a {@link SizeUnit} instance, never <code>null</code>.
   */
  public static SizeUnit toUnit( final double aSize )
  {
    SizeUnit[] sizes = values();
    double size = Math.abs( aSize );

    int i = sizes.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( size >= sizes[i].getFactor() )
      {
        break;
      }
    }

    return sizes[Math.max( i, 0 )];
  }

  /**
   * Returns the given size as string representation using this size unit's
   * display name.
   * 
   * @param aSize
   *          the size to convert to a string representation;
   * @param aScale
   *          the scale (= number of digits after decimal separator) to use in
   *          the string representation.
   * @return a string representation of the given size, like "1.44MB", never
   *         <code>null</code>.
   */
  public String format( final double aSize, final int aScale )
  {
    // For *very* small sizes, we simply always yield zero...
    if ( ( Math.abs( aSize ) < ZERO_THRESHOLD ) && ( this != B ) )
    {
      return B.format( 0.0, aScale );
    }

    final Double size = Double.valueOf( aSize / getFactor() );
    final String format = String.format( "%%.%df%%s", Integer.valueOf( aScale ) );
    return String.format( format, size, getDisplayName() );
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
