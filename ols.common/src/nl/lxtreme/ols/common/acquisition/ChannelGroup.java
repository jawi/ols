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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import java.awt.*;


/**
 * Denotes a group of channels.
 */
public interface ChannelGroup extends Comparable<ChannelGroup>
{
  // METHODS

  /**
   * Returns the number of channels in this channel group.
   * 
   * @return the number of channels in this group, > 0.
   */
  int getChannelCount();

  /**
   * Returns the channels present in this channel group.
   * 
   * @return an array with {@link Channel}s, never <code>null</code>.
   */
  Channel[] getChannels();

  /**
   * Returns the color for this channel group, if specified.
   * 
   * @return a {@link Color}, or <code>null</code> in case the default color
   *         should be used (UI-specific).
   */
  Color getColor();

  /**
   * The logical index of this channel group.
   * 
   * @return an index, >= 0.
   */
  int getIndex();

  /**
   * Returns the bit-mask to use for this channel group that includes all of its
   * channels (bitwise OR).
   * 
   * @return a bit-mask, >= 1.
   */
  int getMask();

  /**
   * The name of this channel group.
   * 
   * @return a name, never <code>null</code>.
   */
  String getName();

  /**
   * Returns the value of this channel group according to a given sample value
   * and the known mask.
   * <p>
   * This method should combine the values of this individual channels in their
   * logical order (= order defined).
   * </p>
   * 
   * @param aSampleValue
   *          the sample value to extract this channel group's value from.
   * @return the value of this channel group.
   */
  int getValue( int aSampleValue );

  /**
   * Moves a given channel to this channel group at the given index. If the
   * given channel already belongs to this channel group, it will be moved to
   * the given index.
   * 
   * @param aChannel
   *          the channel to move;
   * @param aIndex
   *          the new index of this channel, &gt;= 0.
   */
  void move( Channel aChannel, int aIndex );

  /**
   * Sets the color for this channel group.
   * 
   * @param aColor
   *          the color to set, can be <code>null</code> if the default
   *          (UI-specific) color should be used.
   */
  void setColor( Color aColor );

  /**
   * Sets the name of this channel group.
   * 
   * @param aName
   *          the name of this channel group, cannot be <code>null</code>.
   */
  void setName( String aName );
}
