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
package nl.lxtreme.ols.client.signaldisplay.util;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.Renderer;


/**
 * Provides a glass pane for use while dragging channels around. This glass pane
 * will show a marker where the drop location of the channel will be.
 */
public final class GhostGlassPane extends JPanel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final GhostGlassPaneModel model;

  private volatile Rectangle affectedArea;
  private volatile Point paintLocation;
  private volatile Renderer renderer;

  // CONSTRUCTORS

  /**
   * Creates a new {@link GhostGlassPane} instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  public GhostGlassPane( final SignalDiagramController aController )
  {
    setOpaque( false );

    this.model = new GhostGlassPaneModel( aController );
  }

  // METHODS

  /**
   * Creates the rendering hints for this view.
   */
  private static RenderingHints createRenderingHints()
  {
    RenderingHints hints = new RenderingHints( RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR );
    hints.put( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    hints.put( RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED );
    return hints;
  }

  /**
   * Repaints only the affected areas of this glass pane.
   * 
   * @see #setDropPoint(Point)
   */
  public void repaintPartially()
  {
    if ( this.affectedArea == null )
    {
      repaint();
    }
    else
    {
      final Rectangle repaintRect = new Rectangle( this.affectedArea );
      // take a slighter larger area in order to ensure we've repainted
      // everything correctly...
      repaintRect.grow( 1, 1 );

      repaint( repaintRect );
    }
  }

  /**
   * Sets the renderer and location where to use it.
   * 
   * @param aRenderer
   *          the renderer to use on this glass pane, cannot be
   *          <code>null</code>;
   * @param aPaintLocation
   *          the location where the to paint the renderer, cannot be
   *          <code>null</code>;
   * @param aContext
   *          the (optional) rendering context.
   */
  public void setRenderer( final Renderer aRenderer, final Point aPaintLocation, final Object... aContext )
  {
    this.paintLocation = aPaintLocation;
    this.renderer = aRenderer;
    setRenderContext( aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setVisible( final boolean aFlag )
  {
    super.setVisible( aFlag );

    if ( !aFlag )
    {
      // Clean up the administration...
      this.paintLocation = null;
      this.renderer = null;
      this.affectedArea = null;
    }
  }

  /**
   * Updates the current renderer with the given location and context
   * information.
   * 
   * @param aPaintLocation
   *          the location where the to paint the renderer, cannot be
   *          <code>null</code>;
   * @param aContext
   *          the (optional) rendering context.
   */
  public void updateRenderer( final Point aPaintLocation, final Object... aContext )
  {
    setRenderer( this.renderer, aPaintLocation, aContext );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( final Graphics aGraphics )
  {
    if ( ( this.paintLocation == null ) || ( this.renderer == null ) || !isVisible() )
    {
      return;
    }

    final Graphics2D g2d = ( Graphics2D )aGraphics.create();
    try
    {
      g2d.setRenderingHints( createRenderingHints() );
      
      g2d.setComposite( this.model.getComposite() );
      g2d.setColor( this.model.getColor() );

      int x = this.paintLocation.x;
      int y = this.paintLocation.y;

      this.affectedArea = this.renderer.render( g2d, x, y );
    }
    finally
    {
      g2d.dispose();
    }
  }

  /**
   * Sets the rendering context for the renderer that should be painted.
   * 
   * @param aParameters
   *          the rendering context parameters, cannot be <code>null</code>.
   */
  private void setRenderContext( final Object... aParameters )
  {
    if ( this.renderer != null )
    {
      this.renderer.setContext( aParameters );
    }
  }
}
