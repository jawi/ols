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
import java.awt.geom.*;

import javax.swing.*;
import javax.swing.plaf.*;

import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides the actual UI implementation for the channel labels.
 */
public class ChannelLabelsUI extends ComponentUI
{
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

  @Override
  public Dimension getPreferredSize( JComponent aComponent )
  {
    ChannelLabelsView view = ( ChannelLabelsView )aComponent;
    ChannelLabelsViewModel model = view.getModel();

    int height = 0;
    int width = 0;
    if ( model.hasData() )
    {
      height = model.getPreferredHeight();
      width = model.getPreferredWidth();
    }

    return new Dimension( width, height );
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

      final IUIElement[] elements = model.getSignalElements( clip.y, clip.height );
      if ( elements.length == 0 )
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
      canvas.translate( 0, elements[0].getYposition() );

      for ( IUIElement element : elements )
      {
        if ( element instanceof ElementGroup )
        {
          paintGroupLabel( canvas, model, ( ElementGroup )element, compWidth );
        }
        else if ( element instanceof SignalElement )
        {
          paintBackground( canvas, model, ( SignalElement )element, compWidth );
          paintSignalLabel( canvas, model, ( SignalElement )element, compWidth );
        }

        canvas.translate( 0, element.getHeight() + spacingY );
      }
    }
    finally
    {
      canvas.dispose();
      canvas = null;
    }
  }

  /**
   * Draws the label (and optionally its drop shadow).
   * 
   * @param aCanvas
   *          the canvas to paint on;
   * @param aModel
   *          the model to use;
   * @param aText
   *          the text to draw;
   * @param aColor
   *          the color to use for drawing the text;
   * @param aXpos
   *          the X position where the text should be drawn;
   * @param aYpos
   *          the Y position where the text should be drawn.
   */
  private void drawLabel( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel, final String aText,
      final Color aColor, final int aXpos, final int aYpos )
  {
    if ( aModel.isDrawTextShadow() )
    {
      aCanvas.setColor( aModel.getTextShadowColor() );
      aCanvas.drawString( aText, aXpos + 2, aYpos + 2 );
    }

    aCanvas.setColor( aColor );
    aCanvas.drawString( aText, aXpos, aYpos );
  }

  /**
   * Returns whether or not the given signal element is the current selected on
   * (by hovering the mouse over it).
   * 
   * @param aElement
   * @param aModel
   * @return <code>true</code> if the given element is the selected element,
   *         <code>false</code> otherwise.
   */
  private boolean isSelectedElement( final IUIElement aElement, final ChannelLabelsViewModel aModel )
  {
    if ( !( aElement instanceof SignalElement ) )
    {
      return false;
    }
    SignalElement signalElement = ( SignalElement )aElement;
    return signalElement.isDigitalSignal()
        && ( aModel.getSelectedChannelIndex() == signalElement.getChannel().getIndex() );
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

    Color color1 = aModel.getLabelGradientColor1();
    Color color2 = aModel.getLabelGradientColor2();
    if ( isSelectedElement( aElement, aModel ) )
    {
      color2 = ColorUtils.getHighlightColor( color2, 2.0f );
    }

    final GradientPaint paint = new GradientPaint( x, y - 5, color2, x, height + 7, color1 );
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
  private void paintSignalLabel( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final SignalElement aElement, final int aWidth )
  {
    String label = aElement.getLabel();
    int aHeight = aElement.getHeight();
    boolean highlight = false;
    Color labelColor = aModel.getSignalLabelForegroundColor();

    String index = "";
    if ( aElement.isDigitalSignal() && aModel.isShowChannelIndex() )
    {
      index = Integer.toString( aElement.getChannel().getIndex() );
    }
    highlight = isSelectedElement( aElement, aModel );

    boolean labelDefined = label != null && !"".equals( label.trim() );
    boolean indexDefined = index != null && !"".equals( index.trim() );

    int padding = aModel.getHorizontalPadding();
    final double middle = ( aHeight / 2.0 );

    if ( labelDefined )
    {
      Font labelFont = aModel.getLabelFont();
      FontMetrics labelFm = aCanvas.getFontMetrics( labelFont );

      Rectangle2D labelBounds = labelFm.getStringBounds( label, aCanvas );

      final int labelXpos = ( int )( aWidth - labelBounds.getWidth() - padding );
      final int labelYpos;
      if ( !indexDefined )
      {
        labelYpos = ( int )( middle - labelBounds.getCenterY() );
      }
      else
      {
        labelYpos = ( int )( middle - labelFm.getDescent() );
      }

      if ( highlight )
      {
        labelColor = ColorUtils.getHighlightColor( labelColor, 2.0f );
      }

      aCanvas.setFont( labelFont );
      drawLabel( aCanvas, aModel, label, labelColor, labelXpos, labelYpos );
    }

    if ( indexDefined )
    {
      Font indexFont = aModel.getIndexFont();
      FontMetrics indexFm = aCanvas.getFontMetrics( indexFont );

      Rectangle2D indexBounds = indexFm.getStringBounds( index, aCanvas );

      final int indexXpos = ( int )( aWidth - indexBounds.getWidth() - padding );
      final int indexYpos;
      if ( !labelDefined )
      {
        indexYpos = ( int )Math.round( middle + indexBounds.getCenterY() );
      }
      else
      {
        indexYpos = ( int )Math.round( middle + indexFm.getAscent() );
      }

      aCanvas.setFont( indexFont );
      drawLabel( aCanvas, aModel, index, aModel.getIndexForegroundColor(), indexXpos, indexYpos );
    }
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
  private void paintGroupLabel( final Graphics2D aCanvas, final ChannelLabelsViewModel aModel,
      final ElementGroup aElement, final int aWidth )
  {
    String label = aElement.getLabel();
    int aHeight = aElement.getHeight();

    Color labelColor = aModel.getGroupLabelForegroundColor();
    boolean highlight = aElement.getDigitalSignalByChannelIndex( aModel.getSelectedChannelIndex() ) != null;
    boolean labelDefined = label != null && !"".equals( label.trim() );

    int padding = aModel.getHorizontalPadding();

    if ( labelDefined )
    {
      Font labelFont = aModel.getLabelFont();
      FontMetrics labelFm = aCanvas.getFontMetrics( labelFont );

      Rectangle2D labelBounds = labelFm.getStringBounds( label, aCanvas );

      final int labelXpos = padding;
      final int labelYpos = ( int )( aHeight - labelBounds.getMaxY() );

      if ( highlight )
      {
        labelColor = ColorUtils.getHighlightColor( labelColor, 2.0f );
      }

      aCanvas.setFont( labelFont );
      drawLabel( aCanvas, aModel, label, labelColor, labelXpos, labelYpos );
    }
  }
}
