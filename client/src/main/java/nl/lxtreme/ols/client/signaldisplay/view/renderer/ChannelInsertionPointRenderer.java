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
 * @author jawi
 */
public class ChannelInsertionPointRenderer extends BaseRenderer
{
  // CONSTANTS

  private static final int CHANNEL_ROW_MARKER_WIDTH = 100;

  private static final Stroke INDICATOR_STROKE = new BasicStroke( 1.5f );

  // VARIABLES

  private String channelLabel;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void setContext( final Object... aParameters )
  {
    if ( ( aParameters == null ) || ( aParameters.length < 1 ) )
    {
      throw new IllegalArgumentException( "Expected one String parameter!" );
    }
    this.channelLabel = ( String )aParameters[0];
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Rectangle render( final Graphics2D aCanvas )
  {
    final FontMetrics fm = aCanvas.getFontMetrics();

    int yPos = fm.getLeading() + fm.getAscent();
    int labelWidth = 0;
    if ( ( this.channelLabel != null ) && !this.channelLabel.trim().isEmpty() )
    {
      labelWidth = fm.stringWidth( this.channelLabel );

      aCanvas.drawString( this.channelLabel, CHANNEL_ROW_MARKER_WIDTH - labelWidth, yPos );
    }

    aCanvas.setStroke( INDICATOR_STROKE );

    aCanvas.drawLine( 0, 0, CHANNEL_ROW_MARKER_WIDTH, 0 );

    return new Rectangle( -2, -2, Math.max( labelWidth, CHANNEL_ROW_MARKER_WIDTH ) + 4, yPos + 4 );
  }
}
