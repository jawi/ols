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
 * Represents a unit of time that can be displayed has a scale factor.
 */
public enum UnitOfTime
{
  // CONSTANTS

  S( "", 1.0 ), //
  MS( "m", 1.0e-3 ), //
  US( "\u03BC", 1.0e-6 ), //
  NS( "n", 1.0e-9 ), //
  PS( "p", 1.0e-12 ), //
  FS( "f", 1.0e-15 ); //

  /**
   * Constant used to determine whether we should show "0.000 s".
   */
  public static final double ZERO_TIME_THRESHOLD = 1.0e-16;

  // VARIABLES

  private String displayName;
  private double factor;

  // CONSTRUCTORS

  /**
   * Creates a new DisplayUtils.UnitOfTime instance.
   */
  private UnitOfTime( final String aPrefix, final double aFactor )
  {
    this.displayName = aPrefix.concat( "s" );
    this.factor = aFactor;
  }

  // METHODS

  /**
   * @param aTimeValue
   *          the time value to convert to a displayable representation.
   * @return a displayable representation of the given time value, never
   *         <code>null</code>.
   */
  public static String toString( final double aTimeValue )
  {
    UnitOfTime unit = valueOf( aTimeValue );
    Double t = Double.valueOf( aTimeValue / unit.getFactor() );
    return String.format( "%.3f%s", t, unit.getDisplayName() );
  }

  /**
   * Converts a given time value (as double representation, in seconds) to a
   * more suitable unit of time.
   * 
   * @param aTimeValue
   *          the time value (in seconds) to return the unit of time for.
   * @return a {@link UnitOfTime}, never <code>null</code>.
   */
  public static UnitOfTime valueOf( final double aTimeValue )
  {
    double absTime = Math.abs( aTimeValue );
    final UnitOfTime[] values = values();

    int i = 0;
    if ( absTime > ZERO_TIME_THRESHOLD )
    {
      for ( ; i < values.length; i++ )
      {
        if ( absTime >= values[i].getFactor() )
        {
          break;
        }
      }
    }

    return values[Math.min( i, values.length - 1 )];
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

  /**
   * Returns the predecessor of this unit of time.
   * 
   * @return the predecessor of this unit of time, which is a factor
   *         <em>larger</em> than this unit of time. Can be <code>null</code> if
   *         no greater unit of time is defined.
   */
  public UnitOfTime predecessor()
  {
    final int i = ordinal();
    return ( i < 1 ) ? null : values()[i - 1];
  }

  /**
   * Returns the successor of this unit of time.
   * 
   * @return the successor of this unit of time, which is a factor
   *         <em>smaller</em> than this unit of time. Can be <code>null</code>
   *         if no smaller unit of time is defined.
   */
  public UnitOfTime successor()
  {
    final UnitOfTime[] values = values();
    int i = ordinal();
    if ( i >= ( values.length - 1 ) )
    {
      return null;
    }
    return values[i + 1];
  }
}
