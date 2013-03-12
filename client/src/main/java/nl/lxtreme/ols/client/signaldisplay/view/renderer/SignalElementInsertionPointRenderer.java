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

import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;


/**
 * Renders where a signal element could be inserted when DnD'ing it.
 */
public class SignalElementInsertionPointRenderer extends BaseRenderer
{
  // CONSTANTS

  private static final int CHANNEL_ROW_MARKER_WIDTH = 100;

  private static final Stroke INDICATOR_STROKE = new BasicStroke( 1.5f );

  // VARIABLES

  private final ChannelLabelsViewModel model;
  private final IUIElement element;
  private volatile Point dropPoint;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalElementInsertionPointRenderer} instance.
   */
  public SignalElementInsertionPointRenderer( final ChannelLabelsViewModel aModel, final IUIElement aElement )
  {
    this.model = aModel;
    this.element = aElement;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void setContext( final Object... aParameters )
  {
    if ( ( aParameters != null ) && ( aParameters.length > 0 ) )
    {
      this.dropPoint = ( Point )aParameters[0];
    }
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

    String label = getLabel();
    if ( !label.isEmpty() )
    {
      labelWidth = fm.stringWidth( label );

      aCanvas.drawString( label, Math.max( 2, CHANNEL_ROW_MARKER_WIDTH - labelWidth ), yPos );
    }

    aCanvas.setStroke( INDICATOR_STROKE );

    aCanvas.drawLine( 0, 0, CHANNEL_ROW_MARKER_WIDTH, 0 );

    return new Rectangle( -2, -2, Math.max( labelWidth, CHANNEL_ROW_MARKER_WIDTH ) + 4, yPos + 4 );
  }

  /**
   * @return
   */
  private String getLabel()
  {
    String result = this.element.getLabel();
    if ( result == null )
    {
      result = "";
    }
    if ( this.dropPoint != null )
    {
      final IUIElement dropElement = this.model.findUIElement( this.dropPoint );
      if ( this.model.acceptDrop( this.element, dropElement ) )
      {
        final ElementGroup channelGroupFor = dropElement.getGroup();
        if ( channelGroupFor != null )
        {
          result = result.concat( "  " ).concat( channelGroupFor.getName() );
        }
      }
    }
    return result.trim();
  }
}
