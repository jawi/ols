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
package nl.lxtreme.ols.client.project.impl;


import java.beans.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.*;


/**
 * Denotes a channel with a label and a color.
 */
public final class ChannelImpl implements Channel
{
  // CONSTANTS

  private static final int MAX_CHANNELS = Ols.MAX_CHANNELS;

  // VARIABLES

  private final int index;
  private final int mask;

  private String label;
  private boolean enabled;

  private final List<Annotation<?>> annotations;
  private final PropertyChangeSupport propertyChangeSupport;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelImpl} instance based on a given channel.
   * 
   * @param aChannel
   *          the channel to copy, cannot be <code>null</code>;
   * @param aRetainAnnotation
   *          <code>true</code> if any annotations should be retained,
   *          <code>false</code> otherwise.
   */
  public ChannelImpl( final Channel aChannel, final boolean aRetainAnnotation )
  {
    if ( aChannel == null )
    {
      throw new IllegalArgumentException( "Channel cannot be null!" );
    }

    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.index = aChannel.getIndex();
    this.mask = aChannel.getMask();
    this.label = aChannel.hasName() ? aChannel.getLabel() : null;
    this.enabled = aChannel.isEnabled();

    this.annotations = new CopyOnWriteArrayList<Annotation<?>>();
    if ( aRetainAnnotation )
    {
      this.annotations.addAll( aChannel.getAnnotations() );
    }
  }

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

    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.index = aChannelIdx;
    this.mask = ( int )( 1L << aChannelIdx );
    this.label = null;
    this.enabled = true;

    this.annotations = new CopyOnWriteArrayList<Annotation<?>>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addAnnotation( final Annotation<?> aAnnotation )
  {
    if ( aAnnotation instanceof DataAnnotation )
    {
      this.annotations.add( aAnnotation );
    }
    else
    {
      if ( !hasName() )
      {
        setLabel( aAnnotation.toString() );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.addPropertyChangeListener( aListener );
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
  public int compareTo( final Channel aChannel )
  {
    return ( this.index - aChannel.getIndex() );
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
  public Collection<Annotation<?>> getAnnotations()
  {
    return Collections.unmodifiableCollection( this.annotations );
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
    if ( ( this.label == null ) || "".equals( this.label.trim() ) )
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
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.removePropertyChangeListener( aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setEnabled( final boolean aEnabled )
  {
    boolean oldEnabled = this.enabled;
    this.enabled = aEnabled;

    this.propertyChangeSupport.fireIndexedPropertyChange( "channelEnabled", this.index, oldEnabled, aEnabled );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setLabel( final String aLabel )
  {
    String oldLabel = this.label;
    this.label = aLabel;

    this.propertyChangeSupport.fireIndexedPropertyChange( "channelLabel", this.index, oldLabel, aLabel );
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
   * Crafts a default channel name for use when a channel has no label set.
   * 
   * @return a channel name, never <code>null</code>.
   */
  private String getDefaultName()
  {
    return String.format( "%s-%d", "Channel", Integer.valueOf( getIndex() ) );
  }
}
