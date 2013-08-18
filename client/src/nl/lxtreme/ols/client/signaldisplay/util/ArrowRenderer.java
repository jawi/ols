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
package nl.lxtreme.ols.client.signaldisplay.util;


import java.awt.*;


/**
 * Renders an arrow with two or three arrow heads.
 */
public class ArrowRenderer
{
  // CONSTANTS

  private static final int LEFT_FACING = 1;
  private static final int RIGHT_FACING = -1;

  public static final int HEAD_WIDTH = 8;
  public static final int HEAD_HEIGHT = 8;

  // METHODS

  /**
   * Renders the arrow head with a starting and ending head, and one head in the
   * denoted mid position.
   * 
   * @param aCanvas
   *          the canvas to draw on;
   * @param aWidth
   *          the total width (in pixels) of the drawn arrow;
   * @param aMidPos
   *          the mid position of the 3rd arrow head.
   */
  public static void render( final Graphics2D aCanvas, final int aWidth, final int aMidPos )
  {
    // When given, show an additional arrowhead to denote the pulse
    // itself, taking care of the "smallest" pulse we're displaying...
    if ( ( aWidth > 0 ) && ( aMidPos >= 0 ) )
    {
      int dir = LEFT_FACING;
      if ( ( aWidth - aMidPos ) > aMidPos )
      {
        dir = RIGHT_FACING;
      }

      drawArrowHead( aCanvas, aMidPos, 0, dir, HEAD_WIDTH, HEAD_HEIGHT );
    }

    drawDoubleHeadedArrow( aCanvas, 0, 0, aWidth, 0, 8, 8 );
  }

  /**
   * Draws a single arrow head
   * 
   * @param aCanvas
   *          the canvas to draw on;
   * @param aXpos
   *          the X position of the arrow head;
   * @param aYpos
   *          the (center) Y position of the arrow head;
   * @param aDirection
   *          {@link #LEFT_FACING} to have a left-facing arrow head,
   *          {@link #RIGHT_FACING} to have a right-facing arrow head;
   * @param aArrowWidth
   *          the total width of the arrow head;
   * @param aArrowHeight
   *          the total height of the arrow head.
   */
  private static void drawArrowHead( final Graphics2D aCanvas, final int aXpos, final int aYpos, final int aDirection,
      final int aArrowWidth, final int aArrowHeight )
  {
    final double halfHeight = aArrowHeight / 2.0;
    final int x1 = aXpos + ( aDirection * aArrowWidth );
    final int y1 = ( int )Math.ceil( aYpos - halfHeight );
    final int y2 = ( int )Math.floor( aYpos + halfHeight );

    final Polygon arrowHead = new Polygon();
    arrowHead.addPoint( aXpos, aYpos );
    arrowHead.addPoint( x1, y1 );
    arrowHead.addPoint( x1, y2 );

    aCanvas.fill( arrowHead );
  }

  /**
   * Draws a double headed arrow with arrow heads of a given width and height.
   * 
   * @param aCanvas
   *          the canvas to draw on;
   * @param aX1
   *          the starting X position of the arrow;
   * @param aY1
   *          the starting Y position of the arrow;
   * @param aX2
   *          the ending X position of the arrow;
   * @param aY2
   *          the ending Y position of the arrow;
   * @param aArrowWidth
   *          the total width of the arrow head;
   * @param aArrowHeight
   *          the total height of the arrow head.
   */
  private static void drawDoubleHeadedArrow( final Graphics2D aCanvas, final int aX1, final int aY1, final int aX2,
      final int aY2, final int aArrowWidth, final int aArrowHeight )
  {
    int x1 = aX1;
    int x2 = aX2;

    final int lineWidth = Math.abs( x2 - x1 );
    final int threshold = ( 2 * aArrowWidth ) + 2;

    if ( lineWidth > threshold )
    {
      drawArrowHead( aCanvas, x1, aY1, LEFT_FACING, aArrowWidth, aArrowHeight );
      // why x2 needs to be shifted by one pixel is beyond me...
      drawArrowHead( aCanvas, x2 + 1, aY2, RIGHT_FACING, aArrowWidth, aArrowHeight );

      x1 += aArrowWidth - 1;
      x2 -= aArrowWidth + 1;
    }

    aCanvas.drawLine( x1, aY1, x2, aY2 );
  }
}
