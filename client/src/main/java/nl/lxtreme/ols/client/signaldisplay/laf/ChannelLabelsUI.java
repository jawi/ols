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

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.Renderer;


/**
 * Provides the actual UI implementation for the channel labels.
 */
public class ChannelLabelsUI extends ComponentUI
{
  // VARIABLES

  private final Renderer renderer = new ChannelLabelRenderer();

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
        if ( signalElement.isSignalGroup() )
        {
          paintSignalGroup( canvas, model, signalElement, compWidth );
        }

        if ( signalElement.isDigitalSignal() )
        {
          paintDigitalSignal( canvas, model, signalElement, compWidth );
        }

        if ( signalElement.isGroupSummary() )
        {
          paintGroupSummary( canvas, model, signalElement, compWidth );
        }

        if ( signalElement.isAnalogSignal() )
        {
          paintAnalogScope( canvas, model, signalElement, compWidth );
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
   * Paints the label for the analog scope.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aSignalElement
   *          the signal element to paint the analog signal for, cannot be
   *          <code>null</code>;
   * @param aWidth
   *          the width in pixels.
   */
  final void paintAnalogScope( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aSignalElement, final int aWidth )
  {
    final int height = aSignalElement.getHeight();

    paintElementBackground( aCanvas, aModel, aSignalElement, height, aWidth );

    aCanvas.setFont( aModel.getLabelFont() );
    aCanvas.setColor( aModel.getLabelForegroundColor() );

    this.renderer.setContext( Integer.valueOf( aWidth ), Integer.valueOf( height ), aSignalElement.getLabel() );
    this.renderer.render( aCanvas, 0, 0 );
  }

  /**
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aSignalElement
   *          the signal element to paint the digital signal for, cannot be
   *          <code>null</code>;
   * @param aWidth
   *          the width of the component, in pixels.
   */
  final void paintDigitalSignal( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aSignalElement, final int aWidth )
  {
    final int height = aSignalElement.getHeight();
    final Channel channel = aSignalElement.getChannel();

    paintElementBackground( aCanvas, aModel, aSignalElement, height, aWidth );

    aCanvas.setFont( aModel.getLabelFont() );
    aCanvas.setColor( aModel.getLabelForegroundColor() );

    this.renderer.setContext( Integer.valueOf( aWidth ), Integer.valueOf( height ), channel.getLabel() );
    this.renderer.render( aCanvas, 0, 0 );
  }

  /**
   * Paints the group summary.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aSignalElement
   *          the signal element to paint the group summary for, cannot be
   *          <code>null</code>;
   * @param aWidth
   *          the width of the component, in pixels.
   */
  final void paintGroupSummary( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aSignalElement, final int aWidth )
  {
    final int height = aSignalElement.getHeight();

    paintElementBackground( aCanvas, aModel, aSignalElement, height, aWidth );

    aCanvas.setFont( aModel.getLabelFont() );
    aCanvas.setColor( aModel.getLabelForegroundColor() );

    this.renderer.setContext( Integer.valueOf( aWidth ), Integer.valueOf( height ), aSignalElement.getLabel() );
    this.renderer.render( aCanvas, 0, 0 );
  }

  /**
   * Paints the signal group.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aSignalElement
   *          the signal element to paint the signal group for, cannot be
   *          <code>null</code>;
   * @param aWidth
   *          the width of the component, in pixels.
   */
  final void paintSignalGroup( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aSignalElement, final int aWidth )
  {
    // final int height = aSignalElement.getHeight();
    // paintElementBackground( aCanvas, aModel, height );

    // aCanvas.setFont( aModel.getLabelFont() );
    // aCanvas.setColor( aModel.getLabelForegroundColor() );
    //
    // this.renderer.setContext( Integer.valueOf( aWidth ), Integer.valueOf(
    // height ), aSignalElement.getLabel() );
    // this.renderer.render( aCanvas, -30, 0 );
  }

  /**
   * Paints the background.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>;
   * @param aHeight
   *          the height of the view, in pixels.
   */
  private void paintElementBackground( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aSignalElement, final int aHeight, final int aWidth )
  {
    final int arcWidth = aModel.getArcWidth();

    final int x = -arcWidth;
    final int y = 0;
    final int width = aWidth + arcWidth;
    final int height = aHeight;

    final Color labelBackgroundColor = aModel.getLabelBackgroundColor();
    final Color newBrighterColor = labelBackgroundColor.brighter();

    final GradientPaint paint = new GradientPaint( x, y - 5, newBrighterColor, x, aHeight + 10, labelBackgroundColor );
    final Paint oldPaint = aCanvas.getPaint();

    aCanvas.setPaint( paint );

    aCanvas.fillRoundRect( x, y, width, height, arcWidth, arcWidth );

    aCanvas.setPaint( oldPaint );
    aCanvas.setColor( aSignalElement.getColor() );

    aCanvas.drawRoundRect( x, y, width, height, arcWidth, arcWidth );
  }
}
