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
package nl.lxtreme.ols.util.swing.component.icon;


import java.awt.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.awt.image.*;

import javax.swing.*;


/**
 * Provides an icon which takes another icon and renders a given text over it.
 * <p>
 * Note: it appears that you need to extend {@link ImageIcon} in order to have
 * Swing render "disabled" (= grayed out) versions of this icon. There is no
 * other reason to do this.
 * </p>
 */
public class TextOverlayIcon extends ImageIcon
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new TextOverlayIcon instance, placing the text in the center of
   * the icon.
   * 
   * @param aIconName
   *          the (symbolic) name of the icon;
   * @param aTextOverlay
   *          the text that is to overlayed;
   * @param aPosition
   *          on of the {@link SwingConstants} values that denotes the overlay
   *          position.
   */
  public TextOverlayIcon( final Icon aIcon, final String aTextOverlay )
  {
    this( aIcon, aTextOverlay, SwingConstants.CENTER );
  }

  /**
   * Creates a new TextOverlayIcon instance, placing the text at the given
   * position.
   * 
   * @param aIconName
   *          the (symbolic) name of the icon;
   * @param aTextOverlay
   *          the text that is to overlayed;
   * @param aPosition
   *          on of the {@link SwingConstants} values that denotes the overlay
   *          position.
   */
  public TextOverlayIcon( final Icon aIcon, final String aTextOverlay, final int aPosition )
  {
    setImage( drawCompoundIcon( aIcon, aTextOverlay, aPosition ) );
  }

  // METHODS

  /**
   * Creates the text outline and positions this at the correct position given
   * the width and height of the canvas we're drawing on.
   * 
   * @param aText
   *          the text layout to use;
   * @param aWidth
   *          the width of the canvas, in pixels;
   * @param aHeight
   *          the height of the canvas, in pixels.
   * @return the text outline (as Shape) correctly positioned.
   */
  private Shape createOutline( final TextLayout aText, final int aPosition, final int aWidth, final int aHeight )
  {
    final Shape outline = aText.getOutline( null );
    final Rectangle textBounds = outline.getBounds();

    float xText;
    switch ( getHorizontalAlignment( aPosition ) )
    {
      case SwingConstants.CENTER:
        xText = ( float )( ( aWidth - textBounds.width ) / 2.0 );
        break;
      case SwingConstants.RIGHT:
        xText = ( aWidth - textBounds.width - 1 );
        break;
      default:
        xText = -textBounds.x; // LEFT
        break;
    }

    float yText;
    switch ( getVerticalAlignment( aPosition ) )
    {
      case SwingConstants.CENTER:
        yText = ( float )( aHeight / 2.0 + aText.getAscent() / 4.0 );
        break;
      case SwingConstants.BOTTOM:
        yText = ( aHeight - 1 );
        break;
      default:
        yText = -textBounds.y; // TOP
        break;
    }

    final AffineTransform shift = AffineTransform.getTranslateInstance( xText - 1, yText - 1 );
    return shift.createTransformedShape( outline );
  }

  /**
   * Creates the compound icon by rendering the original icon + text overlay.
   */
  private BufferedImage drawCompoundIcon( final Icon aIcon, final String aText, final int aPosition )
  {
    final int width = aIcon.getIconWidth();
    final int height = aIcon.getIconHeight();

    final BufferedImage result = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );

    Graphics2D g2d = ( Graphics2D )result.getGraphics();

    try
    {
      g2d.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
      g2d.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );
      g2d.setRenderingHint( RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON );
      g2d.setRenderingHint( RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR );
      g2d.setRenderingHint( RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE );
      g2d.setRenderingHint( RenderingHints.KEY_TEXT_LCD_CONTRAST, Integer.valueOf( 100 ) );
      g2d.setRenderingHint( RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY );

      // Paint the real icon...
      aIcon.paintIcon( null /* aObserver */, g2d, 0, 0 );

      // Paint the overlay text...
      final Font textFont = new Font( Font.DIALOG, Font.BOLD, 16 );
      g2d.setFont( textFont );

      final FontRenderContext frc = g2d.getFontRenderContext();

      final TextLayout text = new TextLayout( aText, textFont, frc );
      final Shape shp = createOutline( text, aPosition, width, height );

      g2d.setColor( Color.WHITE );
      g2d.fill( shp );

      g2d.setComposite( AlphaComposite.DstOver.derive( 0.5f ) );

      g2d.setColor( Color.BLACK );
      g2d.draw( shp );
    }
    finally
    {
      g2d.dispose();
      g2d = null;
    }

    return result;
  }

  /**
   * Returns the horizontal alignment for the overlay text.
   * 
   * @param aPosition
   *          the position of the overlay text, see {@link SwingConstants}.
   * @return a horizontal alignment.
   */
  private int getHorizontalAlignment( final int aPosition )
  {
    int result;
    switch ( aPosition )
    {
      case SwingConstants.NORTH_EAST:
      case SwingConstants.SOUTH_EAST:
      case SwingConstants.EAST:
        result = SwingConstants.RIGHT;
        break;
      case SwingConstants.NORTH_WEST:
      case SwingConstants.SOUTH_WEST:
      case SwingConstants.WEST:
        result = SwingConstants.LEFT;
        break;
      case SwingConstants.NORTH:
      case SwingConstants.SOUTH:
      default:
        result = SwingConstants.CENTER;
        break;
    }
    return result;
  }

  /**
   * Returns the vertical alignment for the overlay text.
   * 
   * @param aPosition
   *          the position of the overlay text, see {@link SwingConstants}.
   * @return a vertical alignment.
   */
  private int getVerticalAlignment( final int aPosition )
  {
    int result;
    switch ( aPosition )
    {
      case SwingConstants.NORTH_EAST:
      case SwingConstants.NORTH_WEST:
      case SwingConstants.NORTH:
        result = SwingConstants.TOP;
        break;
      case SwingConstants.SOUTH_WEST:
      case SwingConstants.SOUTH_EAST:
      case SwingConstants.SOUTH:
        result = SwingConstants.BOTTOM;
        break;
      case SwingConstants.EAST:
      case SwingConstants.WEST:
      default:
        result = SwingConstants.CENTER;
        break;
    }
    return result;
  }
}
