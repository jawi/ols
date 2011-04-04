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
package nl.lxtreme.ols.client.diagram.laf;


import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.diagram.*;
import nl.lxtreme.ols.client.diagram.settings.*;


/**
 * @author jajans
 */
public class DiagramCornerUI extends ComponentUI
{
  // METHODS

  /**
   * @see javax.swing.plaf.ComponentUI#paint(java.awt.Graphics,
   *      javax.swing.JComponent)
   */
  @Override
  public void paint( final Graphics aCanvas, final JComponent aComponent )
  {
    final DiagramCorner corner = ( DiagramCorner )aComponent;

    final DataContainer dataContainer = corner.getDataContainer();
    if ( !dataContainer.hasCapturedData() )
    {
      return;
    }

    final Graphics2D canvas = ( Graphics2D )aCanvas;
    // obtain portion of graphics that needs to be drawn
    final Rectangle clipArea = canvas.getClipBounds();

    final DiagramSettings settings = corner.getDiagramSettings();

    canvas.setColor( settings.getBackgroundColor() );
    canvas.fillRect( clipArea.x, clipArea.y, clipArea.width, clipArea.height );
  }
}
