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
package nl.lxtreme.ols.client.signal;


import java.awt.*;

import javax.swing.*;


/**
 * 
 */
public interface DiagramSettings
{
  // METHODS

  /**
   * Returns the background color.
   * 
   * @return the background, never <code>null</code>.
   */
  public abstract Color getBackgroundColor();

  /**
   * Returns the height of the channels indicator.
   * 
   * @return a channel height, in pixels.
   * @see #getSignalHeight()
   */
  public abstract int getChannelHeight();

  /**
   * Returns the cursor color for the cursor with a given index.
   * 
   * @param aCursorIdx
   *          the cursor index of the cursor to get the color for, >= 0 && < 10.
   * @return the cursor color, never <code>null</code>.
   */
  public abstract Color getCursorColor( final int aCursorIdx );

  /**
   * Returns the grid color.
   * 
   * @return the grid, never <code>null</code>.
   */
  public abstract Color getGridColor();

  /**
   * Returns the group background color.
   * 
   * @return the groupBackground, never <code>null</code>.
   */
  public abstract Color getGroupBackgroundColor();

  /**
   * Returns the groupSettingBoxes.
   * 
   * @return the groupSettingBoxes, never <code>null</code>.
   */
  public abstract JCheckBox[][] getGroupSettingBoxes();

  /**
   * Returns the label color.
   * 
   * @return the label, never <code>null</code>.
   */
  public abstract Color getLabelColor();

  /**
   * Returns the height of the scope indicator.
   * 
   * @return a scope height, in pixels.
   */
  public abstract int getScopeHeight();

  /**
   * Returns the signal color.
   * 
   * @return the signal, never <code>null</code>.
   */
  public abstract Color getSignalColor();

  /**
   * Returns the height of the signal indicator.
   * 
   * @return a signal height, in pixels.
   */
  public abstract int getSignalHeight();

  /**
   * Returns the text color.
   * 
   * @return the text, never <code>null</code>.
   */
  public abstract Color getTextColor();

  /**
   * Returns the time color.
   * 
   * @return the time, never <code>null</code>.
   */
  public abstract Color getTimeColor();

  /**
   * Returns the trigger color.
   * 
   * @return the trigger, never <code>null</code>.
   */
  public abstract Color getTriggerColor();

  public abstract boolean isShowByte( final int aGroup );

  public abstract boolean isShowChannels( final int aGroup );

  public abstract boolean isShowScope( final int aGroup );

}

/* EOF */
