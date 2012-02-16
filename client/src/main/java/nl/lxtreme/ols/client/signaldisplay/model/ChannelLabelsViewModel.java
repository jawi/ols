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


import java.awt.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.channel.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;


/**
 * Provides a model for the {@link ChannelLabelsView}.
 */
public class ChannelLabelsViewModel extends AbstractViewModel
{
  // CONSTANTS

  public static final String COMPONENT_MINIMAL_WIDTH = "channellabels.width.minimal";
  public static final String COMPONENT_BACKGROUND_COLOR = "channellabels.color.background";

  public static final String LABEL_FOREGROUND_COLOR = "channellabels.label.color.foreground";
  public static final String LABEL_BACKGROUND_COLOR = "channellabels.label.color.background";
  public static final String LABEL_FONT = "channellabels.label.font";

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
   * @param aMovedChannel
   *          the channel that is moved;
   * @param aInsertPoint
   *          the signal element that the moved channel is inserted before.
   * @return <code>true</code> if the move is accepted, <code>false</code> if
   *         the move is declined.
   */
  public boolean acceptChannel( final Channel aMovedChannel, final SignalElement aInsertPoint )
  {
    boolean result = false;

    if ( ( aMovedChannel != null ) && ( aInsertPoint != null ) )
    {
      // result = insertChannel.getChannelGroup() ==
      // aMovedChannel.getChannelGroup();
      result = true;
    }

    return result;
  }

  /**
   * Determines the virtual channel row corresponding to the given
   * X,Y-coordinate.
   * 
   * @param aCoordinate
   *          the coordinate to return the channel row for, cannot be
   *          <code>null</code>.
   * @return a channel row index (>= 0), or -1 if the point is nowhere near a
   *         channel row.
   */
  public int findChannelVirtualOffset( final Point aCoordinate )
  {
    SignalElement signalElement = findSignalElement( aCoordinate );
    if ( signalElement != null )
    {
      return signalElement.getYposition() + signalElement.getHeight();
    }
    return -1;
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
  public SignalElement findSignalElement( final Point aCoordinate )
  {
    return getSignalDiagramModel().findSignalElement( aCoordinate );
  }

  /**
   * Returns all available channels.
   * 
   * @return a collection of all channels, never <code>null</code>.
   */
  public final Collection<Channel> getAllChannels()
  {
    return getChannelGroupManager().getAllChannels();
  }

  /**
   * Returns the background color for the channel labels.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getBackgroundColor()
  {
    Color color = UIManager.getColor( COMPONENT_BACKGROUND_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_BACKGROUND_COLOR;
    }
    return color;
  }

  /**
   * Returns the channel group for the given channel.
   * 
   * @param aDropElement
   *          the channel of which to return the channel group, cannot be
   *          <code>null</code>.
   * @return a channel group, never <code>null</code>.
   */
  public ChannelGroup getChannelGroupFor( final SignalElement aDropElement )
  {
    final ChannelGroupManager channelGroupManager = getChannelGroupManager();
    return channelGroupManager.getChannelGroup( aDropElement );
  }

  /**
   * Returns the background color for the labels themselves.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getLabelBackgroundColor()
  {
    Color color = UIManager.getColor( LABEL_BACKGROUND_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_CHANNEL_BACKGROUND_COLOR;
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
    Font font = UIManager.getFont( LABEL_FONT );
    if ( font == null )
    {
      font = LafDefaults.DEFAULT_CHANNEL_LABEL_FONT;
    }
    return font;
  }

  /**
   * Returns the foreground color for the labels themselves.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getLabelForegroundColor()
  {
    Color color = UIManager.getColor( LABEL_FOREGROUND_COLOR );
    if ( color == null )
    {
      color = LafDefaults.DEFAULT_CHANNEL_LABEL_COLOR;
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
    int minWidth = UIManager.getInt( COMPONENT_MINIMAL_WIDTH );
    if ( minWidth <= 0 )
    {
      return LafDefaults.DEFAULT_MINIMAL_CHANNEL_WIDTH;
    }
    return minWidth;
  }

  /**
   * Moves a given channel row to another position.
   * 
   * @param aMovedChannel
   *          the channel that is moved, cannot be <code>null</code>;
   * @param aInsertElement
   *          the channel that the moved channel is inserted before, cannot be
   *          <code>null</code>.
   */
  public void moveChannelRows( final Channel aMovedChannel, final SignalElement aInsertElement )
  {
    final ChannelGroupManager channelGroupManager = getChannelGroupManager();

    channelGroupManager.moveChannel( aMovedChannel, aInsertElement );
  }
}
