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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.diagram.settings;


import java.awt.*;

import nl.lxtreme.ols.api.*;


/**
 * Denotes the various settings used in the diagram.
 */
public interface DiagramSettings extends UserSettings
{
  // INNER TYPES

  /**
   * Denotes which color scheme to use for the diagram.
   */
  public static enum ColorScheme
  {
    LIGHT, //
    DARK, //
    CUSTOM;
  }

  /**
   * Denotes what needs to be colorized, the labels, the signals or the
   * channel-background.
   */
  public static enum ColorTarget
  {
    LABELS, //
    SIGNALS, //
    BACKGROUND;
  }

  /**
   * Denotes how the slope of a signal edge should look like.
   */
  public static enum EdgeSlope
  {
    PERPENDICULAR, NON_PERPENDICULAR;
  }

  /**
   * Denotes where the signal is to be drawn inside a channel.
   */
  public static enum SignalAlignment
  {
    TOP, CENTER, BOTTOM;
  }

  // CONSTANTS

  /**
   * Display a group in 8 channel logic level view.
   */
  static final int DISPLAY_CHANNELS = 1;
  /**
   * Display a group in a 8bit resolution scope view.
   */
  static final int DISPLAY_SCOPE = 2;
  /**
   * Display a group in a 8bit hex value view.
   */
  static final int DISPLAY_BYTE = 4;

  // METHODS

  /**
   * Returns the background color.
   * 
   * @return the background, never <code>null</code>.
   */
  Color getBackgroundColor();

  /**
   * Returns the signal color.
   * 
   * @param aChannelIdx
   *          the index of the channel to get the color for, >= 0 && < 32.
   * @return the signal, never <code>null</code>.
   */
  Color getChannelColor( int aChannelIdx );

  /**
   * Returns the height of the channels indicator.
   * 
   * @return a channel height, in pixels.
   * @see #getSignalHeight()
   */
  int getChannelHeight();

  /**
   * Returns the color scheme to use for drawing the signals, cursors and so on.
   * 
   * @return a color scheme, never <code>null</code>.
   */
  ColorScheme getColorScheme();

  /**
   * Returns what needs to be colored, the labels, the signals or the
   * channel-background.
   * 
   * @return a color target, never <code>null</code>.
   */
  ColorTarget getColorTarget();

  /**
   * Returns the cursor color for the cursor with a given index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to get the color for, >= 0 && < 10.
   * @return the cursor color, never <code>null</code>.
   */
  Color getCursorColor( final int aCursorIdx );

  /**
   * Returns the slope of a signal edge.
   * 
   * @return a signal slope edge, either perpendicular or non perpendicular.
   */
  EdgeSlope getEdgeSlope();

  /**
   * Returns the grid color.
   * 
   * @return the grid, never <code>null</code>.
   */
  Color getGridColor();

  /**
   * Returns the group background color.
   * 
   * @return the groupBackground, never <code>null</code>.
   */
  Color getGroupBackgroundColor();

  /**
   * @return
   */
  Color getGroupByteColor();

  /**
   * Returns the default label color, in case the channel colors are to be
   * applied to the signals.
   * 
   * @return a label color, never <code>null</code>.
   */
  Color getLabelColor();

  /**
   * Returns the scope color.
   * 
   * @return a scope color, never <code>null</code>.
   */
  Color getScopeColor();

  /**
   * Returns the height of the scope indicator.
   * 
   * @return a scope height, in pixels.
   */
  int getScopeHeight();

  /**
   * Returns where the signal is to be drawn in a channel.
   * 
   * @return a signal alignment, never <code>null</code>.
   */
  SignalAlignment getSignalAlignment();

  /**
   * Returns the default signal color, in case the channel colors are to be
   * applied to the labels.
   * 
   * @return a signal color, never <code>null</code>.
   */
  Color getSignalColor();

  /**
   * Returns the height of the signal indicator.
   * 
   * @return a signal height, in pixels.
   */
  int getSignalHeight();

  /**
   * Returns the text color.
   * 
   * @return the text, never <code>null</code>.
   */
  Color getTextColor();

  /**
   * Returns the time color.
   * 
   * @return the time, never <code>null</code>.
   */
  Color getTimeColor();

  /**
   * Returns the trigger color.
   * 
   * @return the trigger, never <code>null</code>.
   */
  Color getTriggerColor();

  /**
   * @param aGroup
   * @return
   */
  boolean isShowByte( final int aGroup );

  /**
   * @param aGroup
   * @return
   */
  boolean isShowChannels( final int aGroup );

  /**
   * Returns whether or not the cursor flags contain the time of the cursor
   * itself.
   * 
   * @return <code>true</code> if the cursor timing is to be shown,
   *         <code>false</code> otherwise.
   */
  boolean isShowCursorTiming();

  /**
   * @param aGroup
   * @return
   */
  boolean isShowScope( final int aGroup );
}

/* EOF */
