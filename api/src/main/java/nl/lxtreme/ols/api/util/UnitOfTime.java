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

  /** seconds. */
  S( "", 1.0 ),
  /** milliseconds. */
  MS( "m", 1.0e-3 ),
  /** microseconds. */
  US( "\u03BC", 1.0e-6 ),
  /** nanoseconds. */
  NS( "n", 1.0e-9 ),
  /** picoseconds. */
  PS( "p", 1.0e-12 ),
  /** femtoseconds. */
  FS( "f", 1.0e-15 );

  /** Constant used to determine whether we should show "0.000 s". */
  public static final double ZERO_THRESHOLD = 1.0e-16;
  /** All units are in seconds. */
  private static final String BASE_UNIT = "s";

  // VARIABLES

  private final String displayName;
  private final double factor;

  // CONSTRUCTORS

  /**
   * Creates a new DisplayUtils.UnitOfTime instance.
   */
  private UnitOfTime( final String aPrefix, final double aFactor )
  {
    this.displayName = aPrefix.concat( BASE_UNIT );
    this.factor = aFactor;
  }

  // METHODS

  /**
   * Convenience method to directly get a displayable represention of a given
   * time value.
   * <p>
   * This method does the same as calling:
   * <code>toUnit( aTime ).format( aTime, 3 );</code>.
   * </p>
   * 
   * @param aTime
   *          the time value (in seconds) to get a displayable representation
   *          for.
   * @return a string representation of the given time, never <code>null</code>.
   */
  public static String format( final double aTime )
  {
    return toUnit( aTime ).format( aTime, 2 );
  }

  /**
   * Converts a given time value (as double representation, in seconds) to a
   * more suitable unit of time.
   * 
   * @param aTimeValue
   *          the time value (in seconds) to return the unit of time for.
   * @return a {@link UnitOfTime}, never <code>null</code>.
   */
  public static UnitOfTime toUnit( final double aTimeValue )
  {
    double absTime = Math.abs( aTimeValue );
    final UnitOfTime[] values = values();

    int i = 0;
    for ( ; i < values.length; i++ )
    {
      if ( absTime >= values[i].getFactor() )
      {
        break;
      }
    }

    return values[Math.min( i, values.length - 1 )];
  }

  /**
   * Returns the given time as string representation using this time unit's
   * display name and a fixed number of digits after the decimal separator.
   * 
   * @param aTime
   *          the time (in seconds) to convert to a string representation;
   * @param aScale
   *          the scale (= number of digits after decimal separator) to use in
   *          the string representation.
   * @return a string representation of the given time, like "1.453ms", never
   *         <code>null</code>.
   */
  public String format( final double aTime, final int aScale )
  {
    // For *very* small sizes, we simply always yield zero...
    if ( ( Math.abs( aTime ) < ZERO_THRESHOLD ) && ( this != S ) )
    {
      return S.format( 0.0, aScale );
    }

    final Double time = Double.valueOf( aTime / getFactor() );
    final String format = String.format( "%%.%df%%s", Integer.valueOf( aScale ) );
    return String.format( format, time, getDisplayName() );
  }

  /**
   * Returns the given time as string representation using this time unit's
   * display name and the least number of digits after the decimal separator.
   * 
   * @param aTime
   *          the time (in seconds) to convert to a string representation;
   * @param aScale
   *          the scale (= number of digits after decimal separator) to use in
   *          the string representation.
   * @return a string representation of the given time, like "1.453ms", never
   *         <code>null</code>.
   */
  public String formatHumanReadable( final double aTime )
  {
    // For *very* small sizes, we simply always yield zero...
    if ( ( Math.abs( aTime ) < ZERO_THRESHOLD ) && ( this != S ) )
    {
      return S.formatHumanReadable( 0.0 );
    }

    final Double time = Double.valueOf( aTime / getFactor() );
    String formattedTime = String.format( "%.9f", time );
    formattedTime = formattedTime.replaceAll( "(\\d)0+$", "$1" );

    return String.format( "%s%s", formattedTime, getDisplayName() );
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
