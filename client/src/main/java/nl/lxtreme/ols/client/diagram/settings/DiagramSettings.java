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


/**
 * Denotes the various settings used in the diagram.
 */
public interface DiagramSettings
{
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
   * Returns the cursor color for the cursor with a given index.
   * 
   * @param aCursorIdx
   *          the index of the cursor to get the color for, >= 0 && < 10.
   * @return the cursor color, never <code>null</code>.
   */
  Color getCursorColor( final int aCursorIdx );

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
   * Returns the label color.
   * 
   * @return the label, never <code>null</code>.
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
   * @param aGroup
   * @return
   */
  boolean isShowScope( final int aGroup );
}

/* EOF */
