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


import static nl.lxtreme.ols.util.ColorUtils.*;

import java.awt.*;
import java.util.*;
import java.util.List;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.*;


/**
 * Denotes a channel with a label and a color.
 */
public final class ChannelImpl implements Comparable<ChannelImpl>, Channel
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

  private final List<Annotation<?>> annotations;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelImpl} instance.
   * 
   * @param aChannelIdx
   *          the index of this channel, >= 0 && < {@value #MAX_CHANNELS}.
   */
  public ChannelImpl( final int aChannelIdx )
  {
    if ( ( aChannelIdx < 0 ) || ( aChannelIdx >= MAX_CHANNELS ) )
    {
      throw new IllegalArgumentException( "Invalid channel index!" );
    }

    this.annotations = new ArrayList<Annotation<?>>();

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
  public void addAnnotation( final Annotation<?> aAnnotation )
  {
    this.annotations.add( aAnnotation );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations()
  {
    this.annotations.clear();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final ChannelImpl aChannel )
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
    if ( ( aObject == null ) || !( aObject instanceof ChannelImpl ) )
    {
      return false;
    }

    ChannelImpl other = ( ChannelImpl )aObject;
    if ( this.index != other.index )
    {
      return false;
    }

    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Annotation<?>[] getAnnotations()
  {
    return this.annotations.toArray( new Annotation<?>[this.annotations.size()] );
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
   * {@inheritDoc}
   */
  @Override
  public Color getColor()
  {
    if ( this.color != null )
    {
      return this.color;
    }
    return this.group.getColor();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getIndex()
  {
    return this.index;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLabel()
  {
    if ( ( this.label == null ) || this.label.trim().isEmpty() )
    {
      return getDefaultName();
    }
    return this.label;
  }

  /**
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled()
  {
    return this.enabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setColor( final Color aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Color cannot be null!" );
    }
    this.color = aColor;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setEnabled( final boolean aEnabled )
  {
    this.enabled = aEnabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
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
