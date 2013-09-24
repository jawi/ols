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
package nl.lxtreme.ols.client.signaldisplay.view.renderer;


import java.awt.*;


/**
 * Provides a zoom-region renderer, that paints a sort of rubber band between
 * two points to indicate which region is zoomed.
 */
public class RubberBandRenderer implements Renderer
{
  // VARIABLES

  private volatile Point current;
  private volatile Rectangle viewRect;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Rectangle render( final Graphics2D aCanvas, final int aXpos, final int aYpos )
  {
    int x1 = aXpos;
    int x2 = aXpos;

    int y1 = this.viewRect.y;
    int y2 = this.viewRect.height;

    // First marker
    aCanvas.drawLine( x1, y1, x1, y1 + y2 );

    if ( this.current != null )
    {
      // Second marker
      x2 = this.current.x;

      aCanvas.drawLine( x2, y1, x2, y1 + y2 );
    }

    return new Rectangle( x1 - 1, y1 + 1, ( x2 - x1 ) + 1, y2 + 1 );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setContext( final Object... aParameters )
  {
    this.current = ( Point )aParameters[0];
    this.viewRect = ( Rectangle )aParameters[1];
  }
}
