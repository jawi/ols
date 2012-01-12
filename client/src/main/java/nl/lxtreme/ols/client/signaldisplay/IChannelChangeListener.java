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
package nl.lxtreme.ols.client.signaldisplay;


import java.util.*;

import nl.lxtreme.ols.client.signaldisplay.channel.*;


/**
 * Provides a listener for channel(group) change events.
 */
public interface IChannelChangeListener extends EventListener
{
  // INNER TYPES

  /**
   * Provides the information of an channel change event.
   */
  public static final class ChannelChangeEvent
  {
    // CONSTANTS

    public static final String PROPERTY_LABEL = "label";
    public static final String PROPERTY_COLOR = "color";
    public static final String PROPERTY_ENABLED = "enabled";

    // VARIABLES

    private final Channel channel;
    private final String propertyName;
    private final Object oldValue;
    private final Object newValue;

    // CONSTRUCTORS

    /**
     * Creates a new ChannelChangeEvent instance.
     */
    public ChannelChangeEvent( final Channel aChannel, final String aPropertyName, final Object aOldValue,
        final Object aNewValue )
    {
      this.channel = aChannel;
      this.propertyName = aPropertyName;
      this.oldValue = aOldValue;
      this.newValue = aNewValue;
    }

    // METHODS

    /**
     * Returns the channel whose property changed.
     * 
     * @return the channel, never <code>null</code>.
     */
    public Channel getChannel()
    {
      return this.channel;
    }

    /**
     * Returns the new value of the property.
     * 
     * @return the new value after the change, can be <code>null</code>.
     */
    public Object getNewValue()
    {
      return this.newValue;
    }

    /**
     * Returns the old value of the property.
     * 
     * @return the old value before the change, can be <code>null</code>.
     */
    public Object getOldValue()
    {
      return this.oldValue;
    }

    /**
     * Returns the name of the changed property, see class constants for the
     * specific properties that can change.
     * 
     * @return the name of the property, never <code>null</code>.
     */
    public String getPropertyName()
    {
      return this.propertyName;
    }
  }

  /**
   * Provides the information of a channel movement.
   */
  public static final class ChannelMoveEvent
  {
    // VARIABLES

    private final Channel channel;
    private final ChannelGroup oldGroup;
    private final int oldPosition;

    // CONSTRUCTORS

    /**
     * Creates a new ChannelMoveEvent instance.
     */
    public ChannelMoveEvent( final Channel aChannel, final ChannelGroup aOldGroup, final int aOldPosition )
    {
      this.channel = aChannel;
      this.oldGroup = aOldGroup;
      this.oldPosition = aOldPosition;
    }

    // METHODS

    /**
     * Returns the channel that is moved.
     * 
     * @return the moved channel, never <code>null</code>.
     */
    public Channel getChannel()
    {
      return this.channel;
    }

    /**
     * Returns the new (after the move) channel group of the channel.
     * 
     * @return the new channel group, never <code>null</code>.
     */
    public ChannelGroup getNewGroup()
    {
      return this.channel.getChannelGroup();
    }

    /**
     * Returns the new (after the move) channel position.
     * 
     * @return the new channel position, >= 0.
     */
    public int getNewPosition()
    {
      return this.channel.getVirtualIndex();
    }

    /**
     * Returns the old (before the move) channel group of the channel.
     * 
     * @return the old channel group, never <code>null</code>.
     */
    public ChannelGroup getOldGroup()
    {
      return this.oldGroup;
    }

    /**
     * Returns the old (before the move) channel position.
     * 
     * @return the old channel position, >= 0.
     */
    public int getOldPosition()
    {
      return this.oldPosition;
    }

    /**
     * Returns whether or not the channel is moved between channel groups.
     * 
     * @return <code>true</code> if the channel moved between channel groups,
     *         <code>false</code> otherwise.
     */
    public boolean isGroupChange()
    {
      // We can safely check for reference!
      return getOldGroup() != getNewGroup();
    }
  }

  // METHODS

  /**
   * Called when a channel itself (ie. one of its properties) is changed.
   * 
   * @param aEvent
   *          the event details, never <code>null</code>.
   */
  void channelChanged( ChannelChangeEvent aEvent );

  /**
   * Called when the channel group structure is (re)defined.
   * 
   * @param aChannelList
   *          the new, immutable, list of assigned channels, never
   *          <code>null</code>.
   */
  void channelGroupStructureChanged( Collection<Channel> aChannelList );

  /**
   * Called when a channel is moved either inside a channel group or between
   * channel groups.
   * 
   * @param aEvent
   *          the event details, never <code>null</code>.
   */
  void channelMoved( ChannelMoveEvent aEvent );

}
