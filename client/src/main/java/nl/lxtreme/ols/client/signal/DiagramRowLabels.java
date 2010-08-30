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
package nl.lxtreme.ols.client.signal;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a component that displays the row headers/labels.
 */
public class DiagramRowLabels extends JComponent implements Scrollable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final int PADDING_X = 2;
  private static final int PADDING_Y = 2;
  private static final int MIN_WIDTH = 30;
  /** The tick increment (in pixels). */
  public static final int ROW_INCREMENT = 20;

  // VARIABLES

  private DiagramSettings settings;
  private final AnnotatedData annotatedData;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramRowLabels( final AnnotatedData aData )
  {
    super();

    this.annotatedData = aData;

    setPreferredSize( new Dimension( MIN_WIDTH, 100 ) );
  }

  // METHODS

  /**
   * @see javax.swing.Scrollable#getPreferredScrollableViewportSize()
   */
  @Override
  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }

  /**
   * @see javax.swing.Scrollable#getScrollableBlockIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableBlockIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.HORIZONTAL )
    {
      return 0;
    }

    return aVisibleRect.height - ROW_INCREMENT;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportHeight()
   */
  @Override
  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportWidth()
   */
  @Override
  public boolean getScrollableTracksViewportWidth()
  {
    return true;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableUnitIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableUnitIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.HORIZONTAL )
    {
      return 0;
    }

    int currentPosition = aVisibleRect.y;
    int maxUnitIncrement = ROW_INCREMENT;

    // Return the number of pixels between currentPosition
    // and the nearest tick mark in the indicated direction.
    if ( aDirection < 0 )
    {
      int newPosition = currentPosition - ( currentPosition / maxUnitIncrement ) * maxUnitIncrement;
      return ( newPosition == 0 ) ? maxUnitIncrement : newPosition;
    }
    else
    {
      return ( ( currentPosition / maxUnitIncrement ) + 1 ) * maxUnitIncrement - currentPosition;
    }
  }

  /**
   * @param aDiagramSettings
   */
  public void setDiagramSettings( final DiagramSettings aDiagramSettings )
  {
    this.settings = aDiagramSettings;
  }

  /**
   * @see javax.swing.JComponent#setPreferredSize(java.awt.Dimension)
   */
  @Override
  public final void setPreferredSize( final Dimension aPreferredSize )
  {
    // Let us only scale in height, not width!
    final int minimalWidth = getMinimalWidth();
    super.setPreferredSize( new Dimension( minimalWidth, aPreferredSize.height ) );
  }

  /**
   * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
   */
  @Override
  protected void paintComponent( final Graphics aGraphics )
  {
    if ( !this.annotatedData.hasCapturedData() )
    {
      return;
    }

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = aGraphics.getClipBounds();
    // for some reason, this component gets scrolled horizontally although it
    // has no reasons to do so. Resetting the X-position & width of the clip-
    // area seems to solve this problem...
    clipArea.x = 0;
    clipArea.width = getWidth();

    int yofs = 0;

    aGraphics.setColor( this.settings.getBackgroundColor() );
    aGraphics.fillRect( clipArea.x, clipArea.y, clipArea.width, clipArea.height );

    final int channelHeight = this.settings.getChannelHeight();
    final int scopeHeight = this.settings.getScopeHeight();

    final FontMetrics fm = aGraphics.getFontMetrics();
    final int textXpos = ( clipArea.width - fm.stringWidth( "88" ) - PADDING_X );
    final int textYpos = ( int )( ( channelHeight + fm.getHeight() ) / 2.0 ) - PADDING_Y;

    final int channels = this.annotatedData.getChannels();
    final int enabledChannels = this.annotatedData.getEnabledChannels();

    for ( int block = 0; ( block < channels / 8 ) && ( block < 4 ); block++ )
    {
      final boolean blockEnabled = ( ( enabledChannels >> ( 8 * block ) ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      if ( this.settings.isShowChannels( block ) )
      {
        for ( int bit = 0; bit < 8; bit++ )
        {
          final int labelIdx = bit + block * 8;

          final int y1 = channelHeight * bit + yofs;

          aGraphics.setColor( this.settings.getGridColor() );
          aGraphics.drawLine( clipArea.x, y1, clipArea.x + clipArea.width, y1 );
          aGraphics.drawLine( clipArea.x, y1 + channelHeight, clipArea.x + clipArea.width, y1 + channelHeight );

          String label = this.annotatedData.getChannelLabel( labelIdx );
          if ( DisplayUtils.isEmpty( label ) )
          {
            label = Integer.toString( labelIdx );
            aGraphics.setColor( this.settings.getTextColor() );
          }
          else
          {
            aGraphics.setColor( this.settings.getLabelColor() );
          }

          final int labelYpos = y1 + textYpos;
          final int labelXpos = ( clipArea.width - fm.stringWidth( label ) - PADDING_X );

          aGraphics.drawString( label, labelXpos, labelYpos );
        }

        yofs += channelHeight * 8;
      }

      // Draw scope-thingie (if available)
      if ( this.settings.isShowScope( block ) )
      {
        aGraphics.setColor( this.settings.getTextColor() );
        aGraphics.drawString( "S" + block, textXpos, yofs + ( scopeHeight + fm.getHeight() ) / 2 );

        yofs += scopeHeight;
      }

      // Draw group-byte (if available)
      if ( this.settings.isShowByte( block ) )
      {
        aGraphics.setColor( this.settings.getGroupBackgroundColor() );
        aGraphics.fillRect( clipArea.x, yofs, clipArea.x + clipArea.width, channelHeight );

        // draw bottom grid line
        aGraphics.setColor( this.settings.getGridColor() );
        aGraphics.drawLine( clipArea.x, yofs, clipArea.x + clipArea.width, yofs );
        aGraphics.drawLine( clipArea.x, yofs + channelHeight, clipArea.x + clipArea.width, yofs + channelHeight );

        aGraphics.setColor( this.settings.getTextColor() );
        aGraphics.drawString( "B" + block, textXpos, yofs + textYpos );

        yofs += channelHeight;
      }
    }
  }

  /**
   * Tries the resize this component to such a width that all labels will
   * properly fit.
   */
  private int getMinimalWidth()
  {
    int minWidth = -1;

    final Font font = getFont();
    if ( font != null )
    {
      final FontMetrics fm = getFontMetrics( getFont() );
      for ( int i = 0; i < AnnotatedData.MAX_CHANNELS; i++ )
      {
        String label = this.annotatedData.getChannelLabel( i );
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

/* EOF */
