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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.waveform;


import static java.awt.RenderingHints.*;
import static javax.swing.UIManager.*;
import static nl.lxtreme.ols.client2.views.UIMgr.*;
import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer.*;

import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type;
import nl.lxtreme.ols.util.swing.*;


/**
 * Represents a component for the labels of each waveform.
 */
public class WaveformLabelComponent extends JComponent
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final WaveformModel model;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformLabelComponent} instance.
   */
  public WaveformLabelComponent( WaveformModel aModel )
  {
    this.model = aModel;

    setOpaque( true );
    // Inherit the popup menu from our parent...
    setInheritsPopupMenu( true );
    // Enable synthetic drag events (even when mouse is outside window)...
    setAutoscrolls( true );
  }

  // METHODS

  /**
   * Creates the rendering hints for this view.
   */
  private static RenderingHints createRenderingHints()
  {
    RenderingHints hints = new RenderingHints( KEY_INTERPOLATION, VALUE_INTERPOLATION_BICUBIC );
    hints.put( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON );
    hints.put( KEY_ALPHA_INTERPOLATION, VALUE_ALPHA_INTERPOLATION_QUALITY );
    hints.put( KEY_COLOR_RENDERING, VALUE_COLOR_RENDER_SPEED );
    hints.put( KEY_RENDERING, VALUE_RENDER_SPEED );
    return hints;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    super.addNotify();

    // Bootstrap the width of this component...
    int width = getPreferredWidth();
    int height = this.model.calculateScreenHeight();

    setPreferredSize( new Dimension( width, height ) );

    revalidate();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void paintComponent( Graphics aGraphics )
  {
    Graphics2D canvas = ( Graphics2D )aGraphics.create();

    try
    {
      Rectangle clip = canvas.getClipBounds();

      WaveformElement[] elements = this.model.getWaveformElements( clip.y, clip.height, LOOSE_MEASURER );
      if ( elements.length == 0 )
      {
        return;
      }

      // Tell Swing how we would like to render ourselves...
      canvas.setRenderingHints( createRenderingHints() );

      canvas.setBackground( getColor( SIGNALVIEW_BACKGROUND_COLOR, Color.WHITE ) );
      canvas.clearRect( clip.x, clip.y, clip.width, clip.height );

      int compWidth = getWidth() - getInt( CHANNELLABELS_GUTTER_WIDTH, 1 );
      int spacingY = getInt( SIGNAL_ELEMENT_SPACING );

      // Start drawing at the correct position in the clipped region...
      canvas.translate( 0, elements[0].getYposition() );

      for ( WaveformElement element : elements )
      {
        Type type = element.getType();
        if ( Type.GROUP.equals( type ) )
        {
          paintGroupLabel( canvas, element, compWidth );
        }
        else
        {
          paintBackground( canvas, element, compWidth );
          paintSignalLabel( canvas, element, compWidth );
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
  private void drawLabel( Graphics2D aCanvas, String aText, Color aColor, int aXpos, int aYpos )
  {
    if ( getBoolean( CHANNELLABELS_DRAW_TEXT_SHADOW ) )
    {
      aCanvas.setColor( getColor( CHANNELLABELS_TEXT_SHADOW_COLOR, Color.BLACK ) );
      aCanvas.drawString( aText, aXpos + 2, aYpos + 2 );
    }

    aCanvas.setColor( aColor );
    aCanvas.drawString( aText, aXpos, aYpos );
  }

  /**
   * Determines the preferred width of this view, based on the current set of
   * channel labels.
   * 
   * @return a width, in pixels.
   */
  private int getPreferredWidth()
  {
    int minWidth = 0;

    BufferedImage dummy = new BufferedImage( 1, 1, BufferedImage.TYPE_INT_ARGB );
    Graphics2D canvas = dummy.createGraphics();

    int padding = ( 2 * getInt( CHANNELLABELS_PADDING, 1 ) ) + getInt( CHANNELLABELS_GUTTER_WIDTH, 1 );

    try
    {
      final FontMetrics fm = canvas.getFontMetrics( UIMgr.getFont( CHANNELLABELS_LABEL_FONT ) );
      for ( WaveformElement element : this.model.getWaveformElements() )
      {
        String label = element.getLabel();
        if ( label == null )
        {
          label = "";
        }
        minWidth = Math.max( minWidth, fm.stringWidth( label ) + padding );
      }
    }
    finally
    {
      canvas.dispose();
      canvas = null;
      dummy = null;
    }

    // And always ensure we've got at least a minimal width...
    minWidth = Math.max( minWidth, getInt( CHANNELLABELS_MINIMAL_WIDTH, 50 ) );

    return minWidth;
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
  private boolean isSelectedElement( WaveformElement aElement )
  {
    return Type.CHANNEL.equals( aElement.getType() ) && ( this.model.getSelectedElement() == aElement );
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
  private boolean isSelectedGroup( WaveformElement aElement )
  {
    WaveformElement selectedElement = this.model.getSelectedElement();
    return ( selectedElement != null ) && selectedElement.getGroupIndex() == aElement.getGroupIndex();
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
  private void paintBackground( Graphics2D aCanvas, WaveformElement aElement, int aWidth )
  {
    final int arcWidth = getInt( CHANNELLABELS_ARC_WIDTH );

    final int x = -arcWidth;
    final int y = 0;
    final int width = aWidth + arcWidth;
    final int height = aElement.getHeight();

    Color color1 = getColor( CHANNELLABELS_LABEL_GRADIENT_COLOR1, Color.WHITE );
    Color color2 = getColor( CHANNELLABELS_LABEL_GRADIENT_COLOR2, Color.LIGHT_GRAY );
    if ( isSelectedElement( aElement ) )
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
  private void paintGroupLabel( Graphics2D aCanvas, WaveformElement aElement, int aWidth )
  {
    String label = aElement.getLabel();
    int aHeight = aElement.getHeight();

    Color labelColor = getColor( CHANNELLABELS_GROUPLABEL_FOREGROUND_COLOR, Color.DARK_GRAY );
    boolean highlight = isSelectedGroup( aElement );
    boolean labelDefined = ( label != null ) && !"".equals( label.trim() );

    int padding = getInt( CHANNELLABELS_PADDING, 1 );

    if ( labelDefined )
    {
      Font labelFont = UIMgr.getFont( CHANNELLABELS_LABEL_FONT );
      FontMetrics labelFm = aCanvas.getFontMetrics( labelFont );

      Rectangle2D labelBounds = labelFm.getStringBounds( label, aCanvas );

      final int labelXpos = padding;
      final int labelYpos = ( int )( aHeight - labelBounds.getMaxY() );

      if ( highlight )
      {
        labelColor = ColorUtils.getHighlightColor( labelColor, 2.0f );
      }

      aCanvas.setFont( labelFont );
      drawLabel( aCanvas, label, labelColor, labelXpos, labelYpos );
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
  private void paintSignalLabel( Graphics2D aCanvas, WaveformElement aElement, int aWidth )
  {
    String label = aElement.getLabel();
    int aHeight = aElement.getHeight();
    boolean highlight = isSelectedElement( aElement );
    Color labelColor = getColor( CHANNELLABELS_SIGNALLABEL_FOREGROUND_COLOR, Color.DARK_GRAY );

    String index = "";
    if ( Type.CHANNEL.equals( aElement.getType() ) && getBoolean( CHANNELLABELS_SHOW_CHANNEL_INDEX ) )
    {
      index = Integer.toString( aElement.getIndex() );
    }

    boolean labelDefined = label != null && !"".equals( label.trim() );
    boolean indexDefined = index != null && !"".equals( index.trim() );

    int padding = getInt( CHANNELLABELS_PADDING, 1 );
    final double middle = ( aHeight / 2.0 );

    if ( labelDefined )
    {
      Font labelFont = UIMgr.getFont( CHANNELLABELS_LABEL_FONT );
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
      drawLabel( aCanvas, label, labelColor, labelXpos, labelYpos );
    }

    if ( indexDefined )
    {
      Font indexFont = UIMgr.getFont( CHANNELLABELS_INDEX_FONT );
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
      drawLabel( aCanvas, index, getColor( CHANNELLABELS_INDEX_COLOR, Color.LIGHT_GRAY ), indexXpos, indexYpos );
    }
  }
}
