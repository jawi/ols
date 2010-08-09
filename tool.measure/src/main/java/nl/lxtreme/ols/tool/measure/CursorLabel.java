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
package nl.lxtreme.ols.tool.measure;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;


/**
 * Denotes a label for displaying cursor data.
 */
public class CursorLabel extends JLabel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final AnnotatedData data;
  private final int index;

  // CONSTRUCTORS

  /**
   * @param aData
   *          the (annotated) captured data to take the cursors from, cannot be
   *          <code>null</code>;
   * @param aCursorIndex
   *          the index of the cursor to get the time for, >= 0 && < 10.
   */
  public CursorLabel( final AnnotatedData aData, final int aCursorIndex )
  {
    this.data = aData;
    this.index = aCursorIndex;
  }

  // METHODS

  /**
   * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
   */
  @Override
  protected void paintComponent( final Graphics aGraphics )
  {
    final FontMetrics fm = aGraphics.getFontMetrics();
    final int textHeight = fm.getHeight() - fm.getDescent();

    aGraphics.setColor( getForeground() );
    aGraphics.drawString( getCursorTimeDisplayValue(), 0, textHeight );
  }

  /**
   * Returns the time of a cursor with a given index as display value.
   * 
   * @return a display value for the cursor time, can be "not set" if the cursor
   *         is not set.
   */
  private String getCursorTimeDisplayValue()
  {
    double cursorTimeValue = this.data.getCursorTimeValue( this.index );
    if ( cursorTimeValue >= 0.0 )
    {
      return DisplayUtils.displayTime( cursorTimeValue );
    }
    return "<not set>";
  }

}
