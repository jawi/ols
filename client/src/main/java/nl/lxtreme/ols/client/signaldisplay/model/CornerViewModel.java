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
package nl.lxtreme.ols.client.signaldisplay.model;


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;


/**
 * 
 */
public class CornerViewModel extends AbstractViewModel
{
  // CONSTRUCTORS

  /**
   * Creates a new CornerModel instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  public CornerViewModel( final SignalDiagramController aController )
  {
    super( aController );
  }

  // METHODS

  /**
   * Returns the background color for the channel labels.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getBackgroundColor()
  {
    Color color = UIManager.getColor( CORNER_BACKGROUND_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }
}
