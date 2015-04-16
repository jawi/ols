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
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.List;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides a model for the {@link ChannelLabelsView}.
 */
public class ChannelLabelsViewModel extends AbstractViewModel
{
  // CONSTRUCTORS

  /**
   * Creates a new ChannelLabelsViewModel instance.
   *
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  public ChannelLabelsViewModel( final SignalDiagramController aController )
  {
    super( aController );
  }

  // METHODS

  /**
   * Determines whether or not the move given channel index can be accepted.
   * <p>
   * Channels can only be moved within a single group.
   * </p>
   *
   * @param aMovedElement
   *          the channel that is moved;
   * @param aInsertPoint
   *          the signal element that the moved channel is inserted before.
   * @return <code>true</code> if the move is accepted, <code>false</code> if
   *         the move is declined.
   */
  public boolean acceptDrop( final IUIElement aMovedElement, final IUIElement aInsertPoint )
  {
    boolean result = false;

    if ( ( aMovedElement != null ) && ( aMovedElement instanceof SignalElement ) && ( aInsertPoint != null ) )
    {
      // result = insertChannel.getChannelGroup() ==
      // aMovedChannel.getChannelGroup();
      result = true;
    }

    return result;
  }

  /**
   * Finds the signal element located at the given X,Y-coordinate.
   *
   * @param aCoordinate
   *          the coordinate to find the signal element for, cannot be
   *          <code>null</code>.
   * @return the signal element at the given X,Y-coordinate, or
   *         <code>null</code> if no such signal element could be found.
   */
  public IUIElement findUIElement( final Point aCoordinate )
  {
    return getSignalDiagramModel().findUIElement( aCoordinate );
  }

  /**
   * Determines the virtual signal element row corresponding to the given
   * X,Y-coordinate.
   *
   * @param aCoordinate
   *          the coordinate to return the channel row for, cannot be
   *          <code>null</code>.
   * @return a channel row index (>= 0), or -1 if the point is nowhere near a
   *         channel row.
   */
  public int findUIElementVirtualOffset( final Point aCoordinate )
  {
    IUIElement signalElement = findUIElement( aCoordinate );
    if ( signalElement != null )
    {
      final int spacing = UIManager.getInt( UIManagerKeys.SIGNAL_ELEMENT_SPACING ) / 2;
      return signalElement.getYposition() + signalElement.getHeight() + spacing;
    }
    return -1;
  }

  /**
   * Returns all available channels.
   *
   * @return a collection of all channels, never <code>null</code>.
   */
  public final Collection<Channel> getAllChannels()
  {
    final Collection<SignalElement> allElements = getSignalElementManager().getAllElements();
    final List<Channel> channels = new ArrayList<Channel>();
    for ( SignalElement element : allElements )
    {
      if ( element.isDigitalSignal() )
      {
        channels.add( element.getChannel() );
      }
    }
    return channels;
  }

  /**
   * Returns the arc width.
   *
   * @return an arc width, in pixels.
   */
  public int getArcWidth()
  {
    return UIManager.getInt( CHANNELLABELS_ARC_WIDTH );
  }

  /**
   * Returns the background color for the channel labels.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getBackgroundColor()
  {
    Color color = UIManager.getColor( CHANNELLABELS_BACKGROUND_COLOR );
    if ( color == null )
    {
      color = Color.BLACK;
    }
    return color;
  }

  /**
   * Returns the foreground color for the labels themselves.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getGroupLabelForegroundColor()
  {
    Color color = UIManager.getColor( CHANNELLABELS_GROUPLABEL_FOREGROUND_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }

  /**
   * Returns the gutter width.
   *
   * @return a width, in pixels.
   */
  public int getGutterWidth()
  {
    return UIManager.getInt( CHANNELLABELS_GUTTER_WIDTH );
  }

  /**
   * @return a horizontal padding, in pixels.
   */
  public int getHorizontalPadding()
  {
    return UIManager.getInt( CHANNELLABELS_PADDING );
  }

  /**
   * Returns the font for the channel indexes.
   *
   * @return a font, never <code>null</code>.
   */
  public Font getIndexFont()
  {
    Font font = UIManager.getFont( CHANNELLABELS_INDEX_FONT );
    if ( font == null )
    {
      final Font labelFont = getLabelFont();
      font = labelFont.deriveFont( 0.75f * labelFont.getSize2D() );
    }
    return font;
  }

  /**
   * Returns the foreground color for the channel indexes.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getIndexForegroundColor()
  {
    Color color = UIManager.getColor( CHANNELLABELS_INDEX_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }

  /**
   * Returns the font for the labels.
   *
   * @return a font, never <code>null</code>.
   */
  public Font getLabelFont()
  {
    Font font = UIManager.getFont( CHANNELLABELS_LABEL_FONT );
    if ( font == null )
    {
      font = UIManager.getFont( "Label.font" );
    }
    return font;
  }

  /**
   * Returns the background color for the labels themselves.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getLabelGradientColor1()
  {
    Color color = UIManager.getColor( CHANNELLABELS_LABEL_GRADIENT_COLOR1 );
    if ( color == null )
    {
      color = Color.BLACK;
    }
    return color;
  }

  /**
   * Returns the background color for the labels themselves.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getLabelGradientColor2()
  {
    Color color = UIManager.getColor( CHANNELLABELS_LABEL_GRADIENT_COLOR2 );
    if ( color == null )
    {
      color = Color.BLACK;
    }
    return color;
  }

  /**
   * Returns the minimal width of the channel labels.
   *
   * @return a minimal width, in pixels.
   */
  public int getMinimalWidth()
  {
    return UIManager.getInt( CHANNELLABELS_MINIMAL_WIDTH );
  }

  /**
   * @return the preferred height of the view, in pixels, > 0.
   */
  public int getPreferredHeight()
  {
    // channel height *always* follows the height of the main component...
    return this.controller.getViewComponent().getHeight();
  }

  /**
   * Determines the preferred width of this view, based on the current set of
   * channel labels.
   *
   * @return a width, in pixels.
   */
  public int getPreferredWidth()
  {
    int minWidth = 0;

    BufferedImage dummy = new BufferedImage( 1, 1, BufferedImage.TYPE_INT_ARGB );
    Graphics2D canvas = dummy.createGraphics();

    int padding = ( 2 * getHorizontalPadding() ) + getGutterWidth();

    try
    {
      final FontMetrics fm = canvas.getFontMetrics( getLabelFont() );
      for ( SignalElement element : getSignalElementManager().getAllElements() )
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
    minWidth = Math.max( minWidth, getMinimalWidth() );

    return minWidth;
  }

  /**
   * Returns the foreground color for the labels themselves.
   *
   * @return a color, never <code>null</code>.
   */
  public Color getSignalLabelForegroundColor()
  {
    Color color = UIManager.getColor( CHANNELLABELS_SIGNALLABEL_FOREGROUND_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }

  /**
   * @return the drop shadow color for labels, never <code>null</code>.
   */
  public Color getTextShadowColor()
  {
    Color color = UIManager.getColor( CHANNELLABELS_TEXT_SHADOW_COLOR );
    if ( color == null )
    {
      color = Color.BLACK;
    }
    return color;
  }

  /**
   * @return <code>true</code> if a shadow should be drawn behind the labels,
   *         <code>false</code> otherwise.
   */
  public boolean isDrawTextShadow()
  {
    return UIManager.getBoolean( CHANNELLABELS_DRAW_TEXT_SHADOW );
  }

  /**
   * @return <code>true</code> if channel indexes should be shown,
   *         <code>false</code> otherwise.
   */
  public boolean isShowChannelIndex()
  {
    return UIManager.getBoolean( CHANNELLABELS_SHOW_CHANNEL_INDEX );
  }

  /**
   * Moves a given channel row to another position.
   *
   * @param aMovedElement
   *          the channel that is moved, cannot be <code>null</code>;
   * @param aInsertElement
   *          the channel that the moved channel is inserted before, cannot be
   *          <code>null</code>.
   */
  public void moveSignalElement( final SignalElement aMovedElement, final IUIElement aInsertElement )
  {
    final SignalElementManager channelGroupManager = getSignalElementManager();

    final ElementGroup oldGroup = aMovedElement.getGroup();
    final ElementGroup newGroup = aInsertElement.getGroup();

    int newIndex;
    if ( aInsertElement instanceof SignalElement )
    {
      SignalElement signalElement = ( SignalElement )aInsertElement;
      int offset = ( oldGroup != newGroup ) ? 1 : 0;
      newIndex = signalElement.getVirtualIndex() + offset;
    }
    else
    {
      newIndex = newGroup.getElementCount();
    }

    channelGroupManager.moveElement( aMovedElement, newGroup, newIndex );
  }
}
