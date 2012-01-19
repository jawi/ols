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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.view.renderer;


import java.awt.*;


/**
 * Renders the channel label + index.
 */
public class ChannelLabelRenderer extends BaseRenderer
{
  // CONSTANTS

  public static final int PADDING_Y = 1;
  public static final int PADDING_X = 15;

  private static final float INDEX_RELATIVE_FONT_SIZE = 0.75f;

  // VARIABLES

  private int width;
  private int height;
  private String channelAnnotation;
  private String channelLabel;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void setContext( final Object... aParameters )
  {
    if ( ( aParameters == null ) || ( aParameters.length < 3 ) )
    {
      throw new IllegalArgumentException( "Expected at least two Integer & one String parameters!" );
    }
    this.width = ( ( Integer )aParameters[0] ).intValue();
    this.height = ( ( Integer )aParameters[1] ).intValue();
    this.channelLabel = ( String )aParameters[2];
    if ( aParameters.length > 3 )
    {
      this.channelAnnotation = ( String )aParameters[3];
    }
    else
    {
      this.channelAnnotation = "";
    }

    // Cleanup...
    if ( this.channelAnnotation == null )
    {
      // No channel annotation given; use an empty string to internally denote
      // this...
      this.channelAnnotation = "";
    }
    if ( this.channelLabel == null )
    {
      // Use the annotation, if one is given, otherwise, use an empty string...
      this.channelLabel = this.channelAnnotation;
    }

    this.channelAnnotation = this.channelAnnotation.trim();
    this.channelLabel = this.channelLabel.trim();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Rectangle render( final Graphics2D aCanvas )
  {
    Font labelFont = aCanvas.getFont();
    FontMetrics labelFm = aCanvas.getFontMetrics();

    // Derive the index font from the label font...
    Font annoFont = labelFont.deriveFont( Font.PLAIN, labelFont.getSize() * INDEX_RELATIVE_FONT_SIZE );
    FontMetrics annoFm = aCanvas.getFontMetrics( annoFont );

    final double middle = ( this.height / 2.0 );

    if ( !this.channelLabel.isEmpty() )
    {
      final int labelXpos = ( this.width - labelFm.stringWidth( this.channelLabel ) - PADDING_X );
      final int labelYpos;
      if ( this.channelAnnotation.isEmpty() )
      {
        labelYpos = ( int )Math.round( middle + ( labelFm.getMaxAscent() / 2.0 ) ) - PADDING_Y;
      }
      else
      {
        labelYpos = ( int )( middle + annoFm.getLeading() );
      }

      aCanvas.drawString( this.channelLabel, labelXpos, labelYpos );
    }

    if ( !this.channelAnnotation.isEmpty() )
    {
      final int annoXpos = ( this.width - annoFm.stringWidth( this.channelAnnotation ) - PADDING_X );
      final int annoYpos;
      if ( this.channelLabel.isEmpty() )
      {
        annoYpos = ( int )Math.round( middle + ( annoFm.getAscent() / 2.0 ) ) - PADDING_Y;
      }
      else
      {
        annoYpos = ( int )( middle + annoFm.getAscent() + annoFm.getDescent() );
      }

      aCanvas.setFont( annoFont );
      aCanvas.drawString( this.channelAnnotation, annoXpos, annoYpos );
    }

    return null;
  }
}
