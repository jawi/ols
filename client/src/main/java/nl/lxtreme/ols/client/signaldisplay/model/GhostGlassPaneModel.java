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
public class GhostGlassPaneModel extends AbstractViewModel
{
  // CONSTRUCTORS

  /**
   * Creates a new GhostGlassPaneModel instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  public GhostGlassPaneModel( final SignalDiagramController aController )
  {
    super( aController );
  }

  // METHODS

  /**
   * Returns the color in which the items on the glasspane are drawn.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getColor()
  {
    Color color = UIManager.getColor( GLASSPANE_FOREGROUND_COLOR );
    if ( color == null )
    {
      color = Color.YELLOW;
    }
    return color;
  }

  /**
   * Returns the alpha composite.
   * 
   * @return a composite, never <code>null</code>.
   */
  public Composite getComposite()
  {
    int percentage = UIManager.getInt( GLASSPANE_TRANSLUCENT_ALPHA );
    float alpha = percentage / 100.0f;
    return AlphaComposite.getInstance( AlphaComposite.SRC_OVER, alpha );
  }
}
