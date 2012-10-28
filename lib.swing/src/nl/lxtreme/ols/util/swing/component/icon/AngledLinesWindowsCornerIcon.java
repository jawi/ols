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
 * 6:03:57 PM
 */
public class AngledLinesWindowsCornerIcon implements Icon
{

  // RGB values discovered using ZoomIn
  private static final Color WHITE_LINE_COLOR = new Color( 255, 255, 255 );
  private static final Color GRAY_LINE_COLOR = new Color( 172, 168, 153 );

  // Dimensions
  private static final int WIDTH = 13;
  private static final int HEIGHT = 13;

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

    g.setColor( WHITE_LINE_COLOR );
    g.drawLine( 0, 12, 12, 0 );
    g.drawLine( 5, 12, 12, 5 );
    g.drawLine( 10, 12, 12, 10 );

    g.setColor( GRAY_LINE_COLOR );
    g.drawLine( 1, 12, 12, 1 );
    g.drawLine( 2, 12, 12, 2 );
    g.drawLine( 3, 12, 12, 3 );

    g.drawLine( 6, 12, 12, 6 );
    g.drawLine( 7, 12, 12, 7 );
    g.drawLine( 8, 12, 12, 8 );

    g.drawLine( 11, 12, 12, 11 );
    g.drawLine( 12, 12, 12, 12 );

  }
}
