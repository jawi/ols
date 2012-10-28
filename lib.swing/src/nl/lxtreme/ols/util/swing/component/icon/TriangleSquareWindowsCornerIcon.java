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
package nl.lxtreme.ols.util.swing.component.icon;


import java.awt.*;

import javax.swing.*;


/**
 * Created by IntelliJ IDEA. User: Jonathan Simon Date: Oct 5, 2004 Time:
 * 6:03:57 PM To change this template use File | Settings | File Templates.
 */
public class TriangleSquareWindowsCornerIcon implements Icon
{

  // RGB values discovered using ZoomIn
  private static final Color THREE_D_EFFECT_COLOR = new Color( 255, 255, 255 );
  private static final Color SQUARE_COLOR_LEFT = new Color( 184, 180, 163 );
  private static final Color SQUARE_COLOR_TOP_RIGHT = new Color( 184, 180, 161 );
  private static final Color SQUARE_COLOR_BOTTOM_RIGHT = new Color( 184, 181, 161 );

  // Dimensions
  private static final int WIDTH = 12;
  private static final int HEIGHT = 12;

  public int getIconHeight()
  {
    return WIDTH;
  }

  public int getIconWidth()
  {
    return HEIGHT;
  }

  public void paintIcon( final Component c, final Graphics g, final int x, final int y )
  {

    // Layout a row and column "grid"
    int firstRow = 0;
    int firstColumn = 0;
    int rowDiff = 4;
    int columnDiff = 4;

    int secondRow = firstRow + rowDiff;
    int secondColumn = firstColumn + columnDiff;
    int thirdRow = secondRow + rowDiff;
    int thirdColumn = secondColumn + columnDiff;

    // Draw the white squares first, so the gray squares will overlap
    draw3dSquare( g, firstColumn + 1, thirdRow + 1 );

    draw3dSquare( g, secondColumn + 1, secondRow + 1 );
    draw3dSquare( g, secondColumn + 1, thirdRow + 1 );

    draw3dSquare( g, thirdColumn + 1, firstRow + 1 );
    draw3dSquare( g, thirdColumn + 1, secondRow + 1 );
    draw3dSquare( g, thirdColumn + 1, thirdRow + 1 );

    // draw the gray squares overlapping the white background squares
    drawSquare( g, firstColumn, thirdRow );

    drawSquare( g, secondColumn, secondRow );
    drawSquare( g, secondColumn, thirdRow );

    drawSquare( g, thirdColumn, firstRow );
    drawSquare( g, thirdColumn, secondRow );
    drawSquare( g, thirdColumn, thirdRow );

  }

  private void draw3dSquare( final Graphics g, final int x, final int y )
  {
    Color oldColor = g.getColor(); // cache the old color
    g.setColor( THREE_D_EFFECT_COLOR ); // set the white color
    g.fillRect( x, y, 2, 2 ); // draw the square
    g.setColor( oldColor ); // reset the old color
  }

  private void drawSquare( final Graphics g, final int x, final int y )
  {
    Color oldColor = g.getColor();
    g.setColor( SQUARE_COLOR_LEFT );
    g.drawLine( x, y, x, y + 1 );
    g.setColor( SQUARE_COLOR_TOP_RIGHT );
    g.drawLine( x + 1, y, x + 1, y );
    g.setColor( SQUARE_COLOR_BOTTOM_RIGHT );
    g.drawLine( x + 1, y + 1, x + 1, y + 1 );
    g.setColor( oldColor );
  }

}
