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
 * factor.
 */
public enum FrequencyUnit
{
  // CONSTANTS

  HZ( "", 1.0 ), //
  KHZ( "k", 1.0e3 ), //
  MHZ( "M", 1.0e6 ), //
  GHZ( "G", 1.0e9 ), //
  THZ( "T", 1.0e12 ); //

  // VARIABLES

  private String displayName;
  private double factor;

  // CONSTRUCTORS

  /**
   * Creates a new {@link FrequencyUnit} instance.
   */
  private FrequencyUnit( final String aPrefix, final double aFactor )
  {
    this.displayName = aPrefix.concat( "Hz" );
    this.factor = aFactor;
  }

  // METHODS

  /**
   * Convenience method to directly get a displayable represention of a given
   * frequency.
   * 
   * @param aFrequency
   *          the frequency to get a displayable representation for.
   * @return a string representation of the given frequency, never
   *         <code>null</code>.
   */
  public static String toString( final double aFrequency )
  {
    final FrequencyUnit unit = valueOf( aFrequency );
    final Double f = Double.valueOf( aFrequency / unit.getFactor() );
    return String.format( "%.3f%s", f, unit.getDisplayName() );
  }

  /**
   * Converts a given period to a {@link FrequencyUnit} instance.
   * 
   * @param aFrequency
   *          the frequency to convert to a {@link FrequencyUnit}.
   * @return a {@link FrequencyUnit} instance, never <code>null</code>.
   */
  public static FrequencyUnit valueOf( final double aFrequency )
  {
    final FrequencyUnit[] freqs = values();

    int i = freqs.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( aFrequency >= freqs[i].getFactor() )
      {
        break;
      }
    }

    return freqs[Math.max( i, 0 )];
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
