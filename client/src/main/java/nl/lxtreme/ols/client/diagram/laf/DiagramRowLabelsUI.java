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
import nl.lxtreme.ols.client.diagram.settings.DiagramSettings.ColorTarget;
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

  // VARIABLES

  private Font labelFont;
  private Font indexFont;

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
   * @see javax.swing.plaf.ComponentUI#installUI(javax.swing.JComponent)
   */
  @Override
  public void installUI( final JComponent aComponent )
  {
    Font font = LafHelper.getDefaultFont();
    this.labelFont = font.deriveFont( Font.BOLD );
    this.indexFont = font.deriveFont( font.getSize() * 0.75f );
  }

  /**
   * @see javax.swing.plaf.ComponentUI#paint(java.awt.Graphics,
   *      javax.swing.JComponent)
   */
  @Override
  public void paint( final Graphics aCanvas, final JComponent aComponent )
  {
    final Graphics2D canvas = ( Graphics2D )aCanvas;

    final DiagramRowLabels rowLabels = ( DiagramRowLabels )aComponent;
    final DataContainer dataContainer = rowLabels.getDataContainer();

    if ( !dataContainer.hasCapturedData() )
    {
      return;
    }

    final Diagram diagram = rowLabels.getDiagram();
    final DiagramSettings settings = diagram.getDiagramSettings();

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = canvas.getClipBounds();
    // for some reason, this component gets scrolled horizontally although it
    // has no reasons to do so. Resetting the X-position & width of the clip-
    // area seems to solve this problem...
    clipArea.x = 0;
    clipArea.width = rowLabels.getWidth();

    int yofs = 0;

    final int channelHeight = settings.getChannelHeight();
    final int scopeHeight = settings.getScopeHeight();

    final FontMetrics fm = aComponent.getFontMetrics( this.labelFont );
    final FontMetrics indexFm = aComponent.getFontMetrics( this.indexFont );
    final int textXpos = ( clipArea.width - fm.stringWidth( "88" ) - PADDING_X );
    final int textYpos = ( int )( ( channelHeight + fm.getHeight() ) / 2.5 ) - PADDING_Y;
    final int indexYpos = ( int )( ( channelHeight + fm.getHeight() + indexFm.getHeight() ) / 2.0 ) - PADDING_Y;

    final int enabledChannels = dataContainer.getEnabledChannels();

    // Draw top grid line...
    canvas.drawLine( clipArea.x, 0, clipArea.x + clipArea.width, 0 );
    canvas.setFont( this.labelFont );

    final int blockCnt = dataContainer.getBlockCount();
    for ( int block = 0; block < blockCnt; block++ )
    {
      final int channelsOffset = CapturedData.CHANNELS_PER_BLOCK * block;
      final boolean blockEnabled = ( ( enabledChannels >> channelsOffset ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      if ( settings.isShowChannels( block ) )
      {
        final int channelsPerBlock = dataContainer.getChannelsForBlock( block );
        for ( int bit = 0; bit < channelsPerBlock; bit++ )
        {
          final int labelIdx = bit + channelsOffset;

          final Color labelColor = getLabelColor( labelIdx, settings );

          final int y1 = channelHeight * bit + yofs;
          // Paint the (optional) channel background...
          paintChannelBackground( canvas, settings, clipArea, labelIdx, y1 );

          canvas.setColor( settings.getGridColor() );
          canvas.drawLine( clipArea.x, y1 + channelHeight - 1, clipArea.x + clipArea.width, y1 + channelHeight - 1 );

          String indexStr = Integer.toString( labelIdx );
          String label = dataContainer.getChannelLabel( labelIdx );
          if ( StringUtils.isEmpty( label ) )
          {
            label = indexStr;
          }

          final int labelYpos = y1 + textYpos;
          final int labelXpos = ( clipArea.width - fm.stringWidth( label ) - PADDING_X );

          canvas.setColor( labelColor );
          canvas.drawString( label, labelXpos, labelYpos );

          // paint the channel number below the label
          canvas.setFont( this.indexFont );
          int indexXPos = ( clipArea.width - indexFm.stringWidth( indexStr ) - PADDING_X );
          int indexYPos = y1 + indexYpos;
          canvas.drawString( indexStr, indexXPos, indexYPos );
          canvas.setFont( this.labelFont );
        }

        yofs += channelHeight * CapturedData.CHANNELS_PER_BLOCK;
      }

      // Draw scope-thingie (if available)
      if ( settings.isShowScope( block ) )
      {
        // draw background...
        aCanvas.setColor( settings.getBackgroundColor() );
        aCanvas.fillRect( clipArea.x, yofs, clipArea.width, scopeHeight );

        canvas.setColor( settings.getTextColor() );
        canvas.drawString( "S" + block, textXpos, yofs + ( scopeHeight + fm.getHeight() ) / 2 );

        // draw bottom grid line
        canvas.setColor( settings.getGridColor() );
        canvas.drawLine( clipArea.x, yofs + scopeHeight - 1, clipArea.x + clipArea.width, yofs + scopeHeight - 1 );

        yofs += scopeHeight;
      }

      // Draw group-byte (if available)
      if ( settings.isShowByte( block ) )
      {
        canvas.setColor( settings.getGroupBackgroundColor() );
        canvas.fillRect( clipArea.x, yofs, clipArea.width, channelHeight );

        // draw bottom grid line
        canvas.setColor( settings.getGridColor() );
        canvas.drawLine( clipArea.x, yofs + channelHeight - 1, clipArea.x + clipArea.width, yofs + channelHeight - 1 );

        canvas.setColor( settings.getTextColor() );
        canvas.drawString( "B" + block, textXpos, yofs + textYpos );

        yofs += channelHeight;
      }
    }
  }

  /**
   * @param aChannelIdx
   * @param aSettings
   * @return
   */
  private Color getLabelColor( final int aChannelIdx, final DiagramSettings aSettings )
  {
    Color result = aSettings.getLabelColor();
    if ( ColorTarget.LABELS.equals( aSettings.getColorTarget() ) )
    {
      result = aSettings.getChannelColor( aChannelIdx );
    }
    return result;
  }

  /**
   * Tries the resize this component to such a width that all labels will
   * properly fit.
   */
  private int getMinimalWidth( final DiagramRowLabels aRowLabels )
  {
    final DataContainer dataContainer = aRowLabels.getDataContainer();

    int minWidth = -1;

    final FontMetrics fm = aRowLabels.getFontMetrics( this.labelFont );
    for ( int i = 0; i < CapturedData.MAX_CHANNELS; i++ )
    {
      String label = dataContainer.getChannelLabel( i );
      if ( StringUtils.isEmpty( label ) )
      {
        label = "W88";
      }
      minWidth = Math.max( minWidth, fm.stringWidth( label ) );
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

  /**
   * @see LafHelper#paintChannelBackground(Graphics2D, DiagramSettings,
   *      Rectangle, int, int)
   */
  private void paintChannelBackground( final Graphics2D aCanvas, final DiagramSettings aSettings,
      final Rectangle aClipArea, final int aChannelIdx, final int aYoffset )
  {
    LafHelper.paintChannelBackground( aCanvas, aSettings, aClipArea, aChannelIdx, aYoffset );
  }
}
