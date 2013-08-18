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
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;

import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Some commonly used methods.
 */
final class ViewUtils
{
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
      return Unit.Frequency.format( aFrequency.doubleValue() );
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
      return Unit.Frequency.format( 1.0 / aPeriod.doubleValue() );
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
      toolTip = formatTimestamp( aRefTime );
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
      return Unit.Time.format( aTime.doubleValue() );
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
  public static String getToolTipText( final SignalDiagramModel aModel, final double aRefTime )
  {
    return formatReference( aModel.hasTimingData(), aRefTime );
  }

  /**
   * Determines what tooltip is to be displayed.
   * 
   * @param aPoint
   *          a current mouse location, cannot be <code>null</code>.
   * @return a tooltip text, never <code>null</code>.
   */
  public static String getToolTipText( final SignalDiagramModel aModel, final Point aPoint )
  {
    final boolean hasTimingData = aModel.hasTimingData();
    return formatReference( hasTimingData,
        hasTimingData ? aModel.getTimestamp( aPoint.x ) : aModel.locationToSampleIndex( aPoint ) );
  }

  /**
   * Formats a given sample index as state value.
   * 
   * @param aSampleIdx
   *          the sample index to format.
   * @return a state value formatted string, never <code>null</code>.
   */
  private static String formatStateValue( final int aSampleIdx )
  {
    return Integer.toString( aSampleIdx );
  }

  /**
   * Formats a given time as human readable timestamp.
   * 
   * @param aTime
   *          the time to format.
   * @return a timestamp, never <code>null</code>.
   */
  private static String formatTimestamp( final double aTime )
  {
    return Unit.Time.toUnit( aTime ).formatHumanReadable( aTime );
  }

}
