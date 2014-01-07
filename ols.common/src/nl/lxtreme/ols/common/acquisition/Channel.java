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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import java.awt.*;


/**
 * Denotes a single channel in the data set.
 */
public interface Channel extends Comparable<Channel>
{
  // METHODS

  /**
   * Returns the color for this channel, if specified.
   * 
   * @return a {@link Color}, or <code>null</code> in case the default color
   *         should be used (UI-specific).
   */
  Color getColor();

  /**
   * @return the channel group this channel belongs to, cannot be
   *         <code>null</code> (all channels belong to a channel group).
   */
  ChannelGroup getGroup();

  /**
   * Returns the index of this channel.
   * 
   * @return a channel index, >= 0 && < {@value #MAX_CHANNELS}.
   */
  int getIndex();

  /**
   * Returns the (user defined) label for this channel. If no label is set for
   * this channel, this method will return a default name.
   * 
   * @return a label, can be <code>null</code>.
   */
  String getLabel();

  /**
   * Returns the bit-mask to use for this channel.
   * 
   * @return a bit-mask (= always a power of two), >= 1.
   */
  int getMask();

  /**
   * Returns whether or not this channel has a name.
   * 
   * @return <code>true</code> if a name is given to this channel,
   *         <code>false</code> otherwise.
   */
  boolean hasName();

  /**
   * Returns whether or not this channel is "enabled".
   * <p>
   * When a channel is enabled, it is visible in the signal diagram. When
   * disabled, it is masked out from the signal diagram.
   * </p>
   * 
   * @return the enabled
   */
  boolean isEnabled();

  /**
   * Sets the color for this channel.
   * 
   * @param aColor
   *          the color to set, can be <code>null</code> if the default
   *          (UI-specific) color should be used.
   */
  void setColor( Color aColor );

  /**
   * Sets enabled to the given value.
   * 
   * @param aEnabled
   *          the enabled to set.
   */
  void setEnabled( boolean aEnabled );

  /**
   * Sets name to the given value.
   * 
   * @param aName
   *          the name to set.
   */
  void setLabel( String aName );

}
