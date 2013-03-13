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
 * Represents a frequency unit that has a displayable representation and a scale
 * factor to convert it from/to Hertz.
 */
enum FrequencyUnit
{
  // CONSTANTS

  /** millihertz. */
  MiHZ( "m", 1.0e-3 ),
  /** hertz. */
  HZ( "", 1.0 ),
  /** kilohertz. */
  KHZ( "k", 1.0e3 ),
  /** megahertz. */
  MHZ( "M", 1.0e6 ),
  /** gigahertz. */
  GHZ( "G", 1.0e9 ),
  /** terahertz. */
  THZ( "T", 1.0e12 );

  /** Constant used to determine whether we should show "0Hz". */
  public static final double ZERO_THRESHOLD = 1.0e-5;
  /** All units are in Hertz. */
  private static final String BASE_UNIT = "Hz";

  // VARIABLES

  private final String displayName;
  private final double factor;

  // CONSTRUCTORS

  /**
   * Creates a new {@link FrequencyUnit} instance.
   */
  private FrequencyUnit( final String aPrefix, final double aFactor )
  {
    this.displayName = aPrefix.concat( BASE_UNIT );
    this.factor = aFactor;
  }

  // METHODS

  /**
   * Convenience method to directly get a displayable represention of a given
   * frequency.
   * <p>
   * This method does the same as calling:
   * <code>toUnit( aFrequency ).format( aFrequency, 3 );</code>.
   * </p>
   * 
   * @param aFrequency
   *          the frequency to get a displayable representation for.
   * @return a string representation of the given frequency, never
   *         <code>null</code>.
   */
  public static String format( final double aFrequency )
  {
    return toUnit( aFrequency ).format( aFrequency, 3 );
  }

  /**
   * Converts a given period to a {@link FrequencyUnit} instance.
   * 
   * @param aFrequency
   *          the frequency to convert to a {@link FrequencyUnit}.
   * @return a {@link FrequencyUnit} instance, never <code>null</code>.
   */
  public static FrequencyUnit toUnit( final double aFrequency )
  {
    final FrequencyUnit[] freqs = values();
    final double frequency = Math.abs( aFrequency );

    int i = freqs.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( frequency >= freqs[i].getFactor() )
      {
        break;
      }
    }

    return freqs[Math.max( i, 0 )];
  }

  /**
   * Returns the given frequency as string representation using this frequency
   * unit's display name.
   * 
   * @param aFrequency
   *          the frequency to convert to a string representation;
   * @param aScale
   *          the scale (= number of digits after decimal separator) to use in
   *          the string representation.
   * @return a string representation of the given frequency, like "1.234kHz",
   *         never <code>null</code>.
   */
  public String format( final double aFrequency, final int aScale )
  {
    // For *very* small frequencies, we simply always yield zero...
    if ( ( Math.abs( aFrequency ) < ZERO_THRESHOLD ) && ( this != HZ ) )
    {
      return HZ.format( 0.0, aScale );
    }

    final Double frequency = Double.valueOf( aFrequency / getFactor() );
    final String format = String.format( "%%.%df\u00a0%%s", Integer.valueOf( aScale ) );
    return String.format( format, frequency, getDisplayName() );
  }

  /**
   * Returns display name of this frequency unit, like "Hz" or "MHz".
   * 
   * @return a display name, never <code>null</code>.
   */
  public String getDisplayName()
  {
    return this.displayName;
  }

  /**
   * Returns the scale factor to get from Hertz to this frequency unit.
   * 
   * @return a scale factor, >= 1.0.
   */
  public double getFactor()
  {
    return this.factor;
  }
}
