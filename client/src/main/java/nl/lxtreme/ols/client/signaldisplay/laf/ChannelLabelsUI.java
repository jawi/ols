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
package nl.lxtreme.ols.client.signaldisplay.laf;


import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides the actual UI implementation for the channel labels.
 */
public class ChannelLabelsUI extends ComponentUI
{
  // CONSTANTS

  public static final int PADDING_Y = 1;
  public static final int PADDING_X = 15;

  private static final float INDEX_RELATIVE_FONT_SIZE = 0.75f;

  // METHODS

  /**
   * Creates the rendering hints for this view.
   */
  private static RenderingHints createRenderingHints()
  {
    RenderingHints hints = new RenderingHints( RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BICUBIC );
    hints.put( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    hints.put( RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY );
    hints.put( RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_SPEED );
    hints.put( RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED );
    return hints;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void paint( final Graphics aGraphics, final JComponent aComponent )
  {
    final ChannelLabelsView view = ( ChannelLabelsView )aComponent;
    final ChannelLabelsViewModel model = view.getModel();
    if ( !model.hasData() )
    {
      // Nothing to do!
      return;
    }

    Graphics2D canvas = ( Graphics2D )aGraphics.create();

    try
    {
      final Rectangle clip = canvas.getClipBounds();

      final SignalElement[] signalElements = model.getSignalElements( clip.y, clip.height );
      if ( signalElements.length == 0 )
      {
        return;
      }

      // Tell Swing how we would like to render ourselves...
      canvas.setRenderingHints( createRenderingHints() );

      canvas.setBackground( model.getBackgroundColor() );
      canvas.clearRect( clip.x, clip.y, clip.width, clip.height );

      final int compWidth = view.getWidth() - model.getGutterWidth();
      final int spacingY = model.getSignalElementSpacing();

      // Start drawing at the correct position in the clipped region...
      canvas.translate( 0, signalElements[0].getYposition() );

      for ( SignalElement signalElement : signalElements )
      {
        if ( !signalElement.isSignalGroup() )
        {
          paintBackground( canvas, model, signalElement, compWidth );
          paintLabel( canvas, model, signalElement, compWidth );
        }

        canvas.translate( 0, signalElement.getHeight() + spacingY );
      }
    }
    finally
    {
      canvas.dispose();
      canvas = null;
    }
  }

  /**
   * Paints the background.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aElement
   *          the signal element to paint the background for, cannot be
   *          <code>null</code>.
   * @param aWidth
   *          the width of the background, in pixels.
   */
  private void paintBackground( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aElement, final int aWidth )
  {
    final int arcWidth = aModel.getArcWidth();

    final int x = -arcWidth;
    final int y = 0;
    final int width = aWidth + arcWidth;
    final int height = aElement.getHeight();

    final Color labelBackgroundColor = aModel.getLabelBackgroundColor();
    final Color newBrighterColor = labelBackgroundColor.brighter();

    final GradientPaint paint = new GradientPaint( x, y - 5, newBrighterColor, x, height + 10, labelBackgroundColor );
    final Paint oldPaint = aCanvas.getPaint();

    aCanvas.setPaint( paint );

    aCanvas.fillRoundRect( x, y, width, height, arcWidth, arcWidth );

    aCanvas.setPaint( oldPaint );
    aCanvas.setColor( aElement.getColor() );

    aCanvas.drawRoundRect( x, y, width, height, arcWidth, arcWidth );
  }

  /**
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aElement
   *          the signal element to display the label + annotation for, cannot
   *          be <code>null</code>.
   * @param aWidth
   *          the width of the channel label, in pixels.
   */
  private void paintLabel( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel, final SignalElement aElement,
      final int aWidth )
  {
    String label = aElement.getLabel();
    String annotation = ""; // XXX
    int aHeight = aElement.getHeight();

    Font labelFont = aModel.getLabelFont();

    aCanvas.setFont( labelFont );
    aCanvas.setColor( aModel.getLabelForegroundColor() );

    FontMetrics labelFm = aCanvas.getFontMetrics();

    // Derive the index font from the label font...
    Font annoFont = labelFont.deriveFont( Font.PLAIN, labelFont.getSize() * INDEX_RELATIVE_FONT_SIZE );
    FontMetrics annoFm = aCanvas.getFontMetrics( annoFont );

    final double middle = ( aHeight / 2.0 );

    boolean labelDefined = !"".equals( label.trim() );
    boolean annotationDefined = !"".equals( annotation.trim() );

    if ( labelDefined )
    {
      final int labelXpos = ( aWidth - labelFm.stringWidth( label ) - PADDING_X );
      final int labelYpos;
      if ( !annotationDefined )
      {
        labelYpos = ( int )Math.round( middle + ( labelFm.getMaxAscent() / 2.0 ) ) - PADDING_Y;
      }
      else
      {
        labelYpos = ( int )( middle + annoFm.getLeading() );
      }

      aCanvas.drawString( label, labelXpos, labelYpos );
    }

    if ( annotationDefined )
    {
      final int annoXpos = ( aWidth - annoFm.stringWidth( annotation ) - PADDING_X );
      final int annoYpos;
      if ( !labelDefined )
      {
        annoYpos = ( int )Math.round( middle + ( annoFm.getAscent() / 2.0 ) ) - PADDING_Y;
      }
      else
      {
        annoYpos = ( int )( middle + annoFm.getAscent() + annoFm.getDescent() );
      }

      aCanvas.setFont( annoFont );
      aCanvas.drawString( annotation, annoXpos, annoYpos );
    }
  }
}
