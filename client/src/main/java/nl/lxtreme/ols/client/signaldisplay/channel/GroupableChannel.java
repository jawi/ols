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
import java.util.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.*;


/**
 * Denotes a channel which can be grouped.
 */
public class GroupableChannel implements Channel
{
  // VARIABLES

  private final Channel delegate;
  private ChannelGroup group;

  // CONSTRUCTORS

  /**
   * Creates a new {@link GroupableChannel} instance as wrapper for a given
   * {@link Channel}.
   * 
   * @param aChannel
   *          the actual channel, cannot be <code>null</code>.
   */
  public GroupableChannel( final Channel aChannel )
  {
    if ( aChannel == null )
    {
      throw new IllegalArgumentException( "Parameter channel cannot be null!" );
    }
    this.delegate = aChannel;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addAnnotation( final Annotation<?> aAnnotation )
  {
    this.delegate.addAnnotation( aAnnotation );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearAnnotations()
  {
    this.delegate.clearAnnotations();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final Channel aChannel )
  {
    return this.delegate.compareTo( aChannel );
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
    if ( ( aObject == null ) || !( aObject instanceof GroupableChannel ) )
    {
      return false;
    }

    GroupableChannel other = ( GroupableChannel )aObject;
    if ( !this.delegate.equals( other.delegate ) )
    {
      return false;
    }

    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<Annotation<?>> getAnnotations()
  {
    return this.delegate.getAnnotations();
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
    Color result = this.delegate.getColor();
    if ( result == null )
    {
      result = this.group.getColor();
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getIndex()
  {
    return this.delegate.getIndex();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLabel()
  {
    String label = this.delegate.getLabel();
    if ( ( label == null ) || label.trim().isEmpty() )
    {
      return getDefaultName();
    }
    return label;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getMask()
  {
    return this.delegate.getMask();
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
    result = ( prime * result ) + this.delegate.hashCode();
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasName()
  {
    return this.delegate.hasName();
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
    return this.delegate.isEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setColor( final Color aColor )
  {
    this.delegate.setColor( aColor );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setEnabled( final boolean aEnabled )
  {
    this.delegate.setEnabled( aEnabled );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setLabel( final String aName )
  {
    this.delegate.setLabel( aName );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    return getIndex() + ": " + getLabel();
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
