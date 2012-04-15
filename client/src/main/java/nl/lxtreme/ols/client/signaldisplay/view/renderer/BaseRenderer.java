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
 * Provides an abstract base class for a renderer.
 */
abstract class BaseRenderer implements Renderer
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Rectangle render( final Graphics2D aCanvas, final int aXpos, final int aYpos )
  {
    // Move the canvas to the requested position...
    aCanvas.translate( aXpos, aYpos );

    try
    {
      final Rectangle result = render( aCanvas );
      if ( result != null )
      {
        // the resulting rectangle is in the (0, 0) coordinate space, while
        // we're actually at the (aXpos, aYpos) coordinate space...
        result.translate( aXpos, aYpos );
      }
      return result;
    }
    finally
    {
      // Move the canvas back from the requested position...
      aCanvas.translate( -aXpos, -aYpos );
    }
  }

  /**
   * Renders the UI-part on the given canvas. The renderer itself is responsible
   * for determining the absolute coordinates where it should render the
   * UI-part.
   * <p>
   * This method is a convenience for
   * <code>{@link #render(Graphics2D, int, Object...)}</code> with the
   * coordinates (0, 0).
   * </p>
   * 
   * @param aCanvas
   *          the canvas to use to render, never <code>null</code>.
   * @return the rectangle with the coordinates of the affected area on the
   *         given canvas, or <code>null</code> if the entire canvas is
   *         affected.
   */
  protected abstract Rectangle render( final Graphics2D aCanvas );
}
