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
package nl.lxtreme.ols.client.signaldisplay.channel;


import java.awt.*;

import static nl.lxtreme.ols.util.ColorUtils.*;


/**
 * Denotes a channel with a label and a color.
 */
public final class Channel implements Comparable<Channel>
{
  // CONSTANTS

  public static final int MAX_CHANNELS = 32;

  static final Color[] DEFAULT_COLORS = { parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ), //
      parseColor( "7bf9dd" ) //
  };

  // VARIABLES

  private final int index;
  private final int mask;

  private ChannelGroup group;

  private String label;
  private Color color;
  private boolean enabled;

  // TODO annotations!

  // CONSTRUCTORS

  /**
   * Creates a new Channel instance.
   * 
   * @param aChannelIdx
   *          the index of this channel, >= 0 && < {@value #MAX_CHANNELS}.
   */
  public Channel( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx >= MAX_CHANNELS ) )
    {
      throw new IllegalArgumentException( "Invalid channel index!" );
    }

    this.enabled = true;
    this.index = aChannelIdx;
    this.mask = ( int )( 1L << aChannelIdx );
    // Make sure we've got a default color set...
    this.color = DEFAULT_COLORS[aChannelIdx % DEFAULT_COLORS.length];
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final Channel aChannel )
  {
    return this.index - aChannel.index;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof Channel ) )
    {
      return false;
    }

    Channel other = ( Channel )aObject;
    if ( this.index != other.index )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns channel group this channel is assigned to.
   * 
   * @return the channel group, can be <code>null</code> if this channel is not
   *         assigned to a channel group.
   * @see #isAssigned()
   */
  public final ChannelGroup getChannelGroup()
  {
    return this.group;
  }

  /**
   * Returns the color signals should be drawn in.
   * 
   * @return the a signal color, never <code>null</code>.
   */
  public Color getColor()
  {
    if ( this.color != null )
    {
      return this.color;
    }
    return this.group.getColor();
  }

  /**
   * Returns the index of this channel.
   * 
   * @return a channel index, >= 0 && < {@value #MAX_CHANNELS}.
   */
  public int getIndex()
  {
    return this.index;
  }

  /**
   * Returns the (user defined) label for this channel. If no label is set for
   * this channel, this method will return a default name.
   * 
   * @return a label, can be <code>null</code>.
   */
  public String getLabel()
  {
    if ( ( this.label == null ) || this.label.trim().isEmpty() )
    {
      return getDefaultName();
    }
    return this.label;
  }

  /**
   * Returns the bit-mask to use for this channel.
   * 
   * @return a bit-mask (= always a power of two), >= 1.
   */
  public int getMask()
  {
    return this.mask;
  }

  /**
   * Returns the virtual index of this channel.
   * 
   * @return the virtualIndex, >= 0.
   */
  public int getVirtualIndex()
  {
    int result = -1;
    if ( this.group != null )
    {
      result = this.group.getVirtualIndex( this );
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + this.index;
    return result;
  }

  /**
   * Returns whether or not this channel has a name.
   * 
   * @return <code>true</code> if a name is given to this channel,
   *         <code>false</code> otherwise.
   */
  public boolean hasName()
  {
    return ( this.label != null ) && !this.label.trim().isEmpty();
  }

  /**
   * Returns whether or not this channel is assigned to a channel group.
   * 
   * @return <code>true</code> if this channel is assigned to a channel group,
   *         <code>false</code> otherwise.
   */
  public final boolean isAssigned()
  {
    return this.group != null;
  }

  /**
   * Returns whether or not this channel is "enabled".
   * <p>
   * When a channel is enabled, it is visible in the signal diagram. When
   * disabled, it is masked out from the signal diagram.
   * </p>
   * 
   * @return the enabled
   */
  public boolean isEnabled()
  {
    return this.enabled;
  }

  /**
   * Sets color for this channel.
   * 
   * @param aColor
   *          the color to set.
   */
  public void setColor( final Color aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Color cannot be null!" );
    }
    this.color = aColor;
  }

  /**
   * Sets enabled to the given value.
   * 
   * @param aEnabled
   *          the enabled to set.
   */
  public void setEnabled( final boolean aEnabled )
  {
    this.enabled = aEnabled;
  }

  /**
   * Sets name to the given value.
   * 
   * @param aName
   *          the name to set.
   */
  public void setLabel( final String aName )
  {
    this.label = aName;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    return this.index + ": " + getLabel();
  }

  /**
   * If this channel is assigned to a channel group, removes it from that
   * channel group. Otherwise, this method does nothing.
   */
  final void removeChannelGroup()
  {
    this.group = null;
  }

  /**
   * Sets the channel group for this channel.
   * 
   * @param aGroup
   *          the channel group to set, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given group was <code>null</code>;
   * @throws IllegalStateException
   *           in case this channel already has a group assigned.
   */
  final void setChannelGroup( final ChannelGroup aGroup )
  {
    if ( aGroup == null )
    {
      throw new IllegalArgumentException( "Group cannot be null!" );
    }
    if ( this.group != null )
    {
      throw new IllegalStateException( "Channel already belongs to a group!" );
    }
    this.group = aGroup;
  }

  /**
   * Crafts a default channel name for use when a channel has no label set.
   * 
   * @return a channel name, never <code>null</code>.
   */
  private String getDefaultName()
  {
    return String.format( "%s-%d", this.group.getName(), Integer.valueOf( getIndex() + 1 ) );
  }
}
