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
package nl.lxtreme.ols.client.diagram.laf;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.diagram.settings.*;
import nl.lxtreme.ols.client.diagram.settings.DiagramSettings.ColorTarget;


/**
 * @author jajans
 */
final class LafHelper
{
  // METHODS

  /**
   * Returns the default font for the current installed L&F.
   * 
   * @return a font, might be <code>null</code>.
   */
  public static Font getDefaultFont()
  {
    Font defaultFont = ( Font )UIManager.get( "Label.font" );
    if ( defaultFont == null )
    {
      defaultFont = new JLabel().getFont();
    }
    return defaultFont;
  }

  /**
   * @param aCanvas
   *          the canvas to paint on;
   * @param aSettings
   *          the diagram settings to use;
   * @param aClipArea
   *          the clipping area to use;
   * @param aChannelIdx
   *          the channel index to paint the background of;
   * @param aYoffset
   *          the Y-offset to paint the background on the canvas.
   */
  public static void paintChannelBackground( final Graphics2D aCanvas, final DiagramSettings aSettings,
      final Rectangle aClipArea, final int aChannelIdx, final int aYoffset )
  {
    final int channelHeight = aSettings.getChannelHeight();

    if ( ColorTarget.BACKGROUND.equals( aSettings.getColorTarget() ) )
    {
      final Color newBrighterColor = aSettings.getChannelColor( aChannelIdx ).brighter();

      final GradientPaint paint = new GradientPaint( aClipArea.x, aYoffset - 5, newBrighterColor, aClipArea.x, aYoffset
          + ( channelHeight / 3 ), aSettings.getBackgroundColor() );

      aCanvas.setPaint( paint );
    }
    else
    {
      aCanvas.setColor( aSettings.getBackgroundColor() );
    }

    aCanvas.fillRect( aClipArea.x, aYoffset, aClipArea.width, channelHeight - 1 );
  }

}
