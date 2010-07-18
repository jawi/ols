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

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a component that displays the row headers/labels.
 */
public class DiagramRowLabels extends JComponent
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final int  PADDING_X        = 2;
  private static final int  PADDING_Y        = 2;

  // VARIABLES

  private String[]          labels;
  private int               channels;
  private int               enabled;
  private DiagramSettings   settings;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramRowLabels()
  {
    super();

    setDoubleBuffered( false );
    setPreferredSize( new Dimension( 25, 100 ) );

    this.channels = -1;
  }

  // METHODS

  /**
   * @param aCapturedData
   */
  public void setCapturedData( final CapturedData aCapturedData )
  {
    this.channels = aCapturedData.channels;
    this.enabled = aCapturedData.enabledChannels;
  }

  /**
   * Sets the labels to the given value.
   * 
   * @param aLabels
   *          the labels to set, cannot be <code>null</code>.
   */
  public void setDiagramLabels( final String[] aLabels )
  {
    this.labels = aLabels;
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
  public void setPreferredSize( final Dimension aPreferredSize )
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
    if ( this.channels < 0 )
    {
      return;
    }

    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = aGraphics.getClipBounds();

    int yofs = 0;

    aGraphics.setColor( this.settings.getBackgroundColor() );
    aGraphics.fillRect( clipArea.x, clipArea.y, clipArea.width, clipArea.height );

    final int channelHeight = this.settings.getChannelHeight();

    final FontMetrics fm = aGraphics.getFontMetrics();
    final int textXpos = ( clipArea.width - fm.stringWidth( "88" ) - PADDING_X );
    final int textYpos = ( int )( ( channelHeight + fm.getHeight() ) / 2.0 ) - PADDING_Y;

    final int scopeHeight = this.settings.getScopeHeight();

    for ( int block = 0; ( block < this.channels / 8 ) && ( block < 4 ); block++ )
    {
      final boolean blockEnabled = ( ( this.enabled >> ( 8 * block ) ) & 0xff ) != 0;
      if ( !blockEnabled )
      {
        continue;
      }

      // draw channel separators
      if ( this.settings.isShowChannels( block ) )
      {
        for ( int bit = 0; bit < 8; bit++ )
        {
          final int labelIdx = bit + block * 8;

          final int y1 = channelHeight * bit + yofs;

          final int labelYpos = y1 + textYpos;
          final int labelXpos;

          aGraphics.setColor( this.settings.getGridColor() );
          aGraphics.drawLine( clipArea.x, y1, clipArea.x + clipArea.width, y1 );
          aGraphics.drawLine( clipArea.x, y1 + channelHeight, clipArea.x + clipArea.width, y1 + channelHeight );

          if ( ( this.labels != null ) && !DisplayUtils.isEmpty( this.labels[labelIdx] ) )
          {
            labelXpos = PADDING_X;

            aGraphics.setColor( this.settings.getLabelColor() );
            aGraphics.drawString( this.labels[labelIdx], labelXpos, labelYpos );
          }
          else
          {
            final String label = Integer.toString( labelIdx );
            labelXpos = ( clipArea.width - fm.stringWidth( label ) - PADDING_X );

            aGraphics.setColor( this.settings.getTextColor() );
            aGraphics.drawString( label, labelXpos, labelYpos );
          }
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
   * Tries the resize this component to such a width that all labels will properly fit.
   */
  private int getMinimalWidth()
  {
    int minWidth = -1;

    final Font font = getFont();
    if ( font != null )
    {
      final FontMetrics fm = getFontMetrics( getFont() );
      if ( this.labels != null )
      {
        for ( String label : this.labels )
        {
          minWidth = Math.max( minWidth, fm.stringWidth( label ) );
        }
      }
      else
      {
        minWidth = fm.stringWidth( "W88" );
      }
    }

    if ( minWidth < 0 )
    {
      minWidth = 21;
    }

    // Ensure there's room for some padding...
    minWidth += 2 * PADDING_X;

    return minWidth;
  }
}

/* EOF */
