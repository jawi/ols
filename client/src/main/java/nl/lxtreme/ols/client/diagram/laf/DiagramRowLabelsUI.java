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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.diagram.laf;


import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.diagram.*;
import nl.lxtreme.ols.client.diagram.settings.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class DiagramRowLabelsUI extends ComponentUI
{
  // CONSTANTS

  public static final int MIN_WIDTH = 30;
  /** The tick increment (in pixels). */
  public static final int ROW_INCREMENT = 20;

  private static final int PADDING_X = 2;
  private static final int PADDING_Y = 2;

  // METHODS

  /**
   * @see javax.swing.plaf.ComponentUI#getMaximumSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getMaximumSize( final JComponent aC )
  {
    return new Dimension( Short.MAX_VALUE, Short.MAX_VALUE );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#getMinimumSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getMinimumSize( final JComponent aComponent )
  {
    return new Dimension( MIN_WIDTH, 0 );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#getPreferredSize(javax.swing.JComponent)
   */
  @Override
  public Dimension getPreferredSize( final JComponent aComponent )
  {
    final DiagramRowLabels rowLabels = ( DiagramRowLabels )aComponent;
    final Diagram diagram = rowLabels.getDiagram();
    return new Dimension( getMinimalWidth( rowLabels ), diagram.getPreferredSize().height );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#paint(java.awt.Graphics,
   *      javax.swing.JComponent)
   */
  @Override
  public void paint( final Graphics aGraphics, final JComponent aComponent )
  {
    final DiagramRowLabels rowLabels = ( DiagramRowLabels )aComponent;
    final DataContainer dataContainer = rowLabels.getDataContainer();

    if ( !dataContainer.hasCapturedData() )
    {
      return;
    }

    final Diagram diagram = rowLabels.getDiagram();
    final DiagramSettings settings = diagram.getDiagramSettings();

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = aGraphics.getClipBounds();
    // for some reason, this component gets scrolled horizontally although it
    // has no reasons to do so. Resetting the X-position & width of the clip-
    // area seems to solve this problem...
    clipArea.x = 0;
    clipArea.width = rowLabels.getWidth();

    int yofs = 0;

    aGraphics.setColor( settings.getBackgroundColor() );
    aGraphics.fillRect( clipArea.x, clipArea.y, clipArea.width, clipArea.height );

    final int channelHeight = settings.getChannelHeight();
    final int scopeHeight = settings.getScopeHeight();

    final FontMetrics fm = aGraphics.getFontMetrics();
    final int textXpos = ( clipArea.width - fm.stringWidth( "88" ) - PADDING_X );
    final int textYpos = ( int )( ( channelHeight + fm.getHeight() ) / 2.0 ) - PADDING_Y;

    final int channels = dataContainer.getChannels();
    final int enabledChannels = dataContainer.getEnabledChannels();

    // Draw top grid line...
    aGraphics.drawLine( clipArea.x, 0, clipArea.x + clipArea.width, 0 );

    for ( int block = 0; ( block < channels / 8 ) && ( block < 4 ); block++ )
    {
      final boolean blockEnabled = ( ( enabledChannels >> ( 8 * block ) ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      if ( settings.isShowChannels( block ) )
      {
        for ( int bit = 0; bit < 8; bit++ )
        {
          final int labelIdx = bit + block * 8;

          final int y1 = channelHeight * bit + yofs;

          aGraphics.setColor( settings.getGridColor() );
          aGraphics.drawLine( clipArea.x, y1 + channelHeight - 1, clipArea.x + clipArea.width, y1 + channelHeight - 1 );

          String label = dataContainer.getChannelLabel( labelIdx );
          if ( DisplayUtils.isEmpty( label ) )
          {
            label = Integer.toString( labelIdx );
            aGraphics.setColor( settings.getTextColor() );
          }
          else
          {
            aGraphics.setColor( settings.getLabelColor() );
          }

          final int labelYpos = y1 + textYpos;
          final int labelXpos = ( clipArea.width - fm.stringWidth( label ) - PADDING_X );

          aGraphics.drawString( label, labelXpos, labelYpos );
        }

        yofs += channelHeight * 8;
      }

      // Draw scope-thingie (if available)
      if ( settings.isShowScope( block ) )
      {
        aGraphics.setColor( settings.getTextColor() );
        aGraphics.drawString( "S" + block, textXpos, yofs + ( scopeHeight + fm.getHeight() ) / 2 );

        // draw bottom grid line
        aGraphics.setColor( settings.getGridColor() );
        aGraphics.drawLine( clipArea.x, yofs + scopeHeight - 1, clipArea.x + clipArea.width, yofs + scopeHeight - 1 );

        yofs += scopeHeight;
      }

      // Draw group-byte (if available)
      if ( settings.isShowByte( block ) )
      {
        aGraphics.setColor( settings.getGroupBackgroundColor() );
        aGraphics.fillRect( clipArea.x, yofs, clipArea.x + clipArea.width, channelHeight );

        // draw bottom grid line
        aGraphics.setColor( settings.getGridColor() );
        aGraphics
            .drawLine( clipArea.x, yofs + channelHeight - 1, clipArea.x + clipArea.width, yofs + channelHeight - 1 );

        aGraphics.setColor( settings.getTextColor() );
        aGraphics.drawString( "B" + block, textXpos, yofs + textYpos );

        yofs += channelHeight;
      }
    }
  }

  /**
   * Tries the resize this component to such a width that all labels will
   * properly fit.
   */
  private int getMinimalWidth( final DiagramRowLabels aRowLabels )
  {
    final DataContainer dataContainer = aRowLabels.getDataContainer();

    int minWidth = -1;

    final Font font = aRowLabels.getFont();
    if ( font != null )
    {
      final FontMetrics fm = aRowLabels.getFontMetrics( font );
      for ( int i = 0; i < DataContainer.MAX_CHANNELS; i++ )
      {
        String label = dataContainer.getChannelLabel( i );
        if ( DisplayUtils.isEmpty( label ) )
        {
          label = "W88";
        }
        minWidth = Math.max( minWidth, fm.stringWidth( label ) );
      }
    }

    // Ensure there's room for some padding...
    minWidth += 2 * PADDING_X;

    // And always ensure we've got at least a minimal width...
    if ( minWidth < MIN_WIDTH )
    {
      minWidth = MIN_WIDTH;
    }

    return minWidth;
  }
}
