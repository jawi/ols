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
package nl.lxtreme.ols.client2.views;


import nl.lxtreme.ols.common.Unit.*;


/**
 * Utility methods for use in views.
 */
public final class ViewUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link ViewUtils} instance.
   */
  private ViewUtils()
  {
    // Nop
  }

  // METHODS

  /**
   * Returns the duty cycle as formatted String value.
   * 
   * @return a String representation, never <code>null</code>.
   */
  public static String formatDutyCycle( final Double aDC )
  {
    if ( aDC != null )
    {
      return String.format( "%.1f %%", aDC );
    }
    return "-";
  }

  /**
   * Returns the given double value as frequency string.
   * 
   * @return a frequency representation.
   */
  public static String formatFrequency( final Double aFrequency )
  {
    if ( aFrequency != null )
    {
      Value freq = Value.asFrequency( aFrequency );
      return String.format( "%#.3s", freq );
    }
    return "-";
  }

  /**
   * Returns the given double value as frequency string.
   * 
   * @return a frequency representation.
   */
  public static String formatPeriodAsFrequency( final Double aPeriod )
  {
    if ( aPeriod != null )
    {
      return formatFrequency( 1.0 / aPeriod.doubleValue() );
    }
    return "-";
  }

  /**
   * Determines what tooltip is to be displayed.
   * 
   * @param aPoint
   *          a current mouse location, cannot be <code>null</code>.
   * @return a tooltip text, never <code>null</code>.
   */
  public static String formatReference( final boolean aHasTimingInformation, final double aRefTime )
  {
    final String toolTip;
    if ( aHasTimingInformation )
    {
      toolTip = formatTime( aRefTime );
    }
    else
    {
      toolTip = formatStateValue( ( int )aRefTime );
    }

    return toolTip;
  }

  /**
   * Returns the given double value as time string.
   * 
   * @return a time representation.
   */
  public static String formatTime( final Double aTime )
  {
    if ( aTime != null )
    {
      return String.format( "%#.2s", Value.asTime( aTime ) );
    }
    return "-";
  }

  /**
   * Formats a given sample index as state value.
   * 
   * @param aSampleIdx
   *          the sample index to format.
   * @return a state value formatted string, never <code>null</code>.
   */
  private static String formatStateValue( int aSampleIdx )
  {
    return Integer.toString( aSampleIdx );
  }

}
