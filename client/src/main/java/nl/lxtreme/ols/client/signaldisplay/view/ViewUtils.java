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
   * Determines what tooltip is to be displayed.
   * 
   * @param aPoint
   *          a current mouse location, cannot be <code>null</code>.
   * @return a tooltip text, never <code>null</code>.
   */
  public static String getToolTipText( final SignalDiagramModel aModel, final Point aPoint )
  {
    String toolTip;
    if ( aModel.hasTimingData() )
    {
      final double refTime = aModel.getCursorTime( aPoint );
      toolTip = UnitOfTime.toUnit( refTime ).format( refTime, 6 );
    }
    else
    {
      final int refIndex = aModel.locationToSampleIndex( aPoint );
      Integer sampleIdx = Integer.valueOf( refIndex );
      toolTip = String.format( "Sample: %d", sampleIdx );
    }

    return toolTip;
  }

}
