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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.common.acquisition;


import java.awt.*;
import java.util.*;
import java.util.List;

import nl.lxtreme.ols.common.acquisition.ChannelBuilder.ChannelImpl;


/**
 * Provides a builder for creating channel groups.
 */
public class ChannelGroupBuilder
{
  // INNER TYPES

  /**
   * Provides a default implementation of {@link ChannelGroup}.
   */
  static final class ChannelGroupImpl implements ChannelGroup
  {
    // VARIABLES

    private final int index;
    private final List<Channel> channels;
    private Color color;
    private String name;

    /**
     * Creates a new {@link ChannelGroupImpl} instance.
     */
    ChannelGroupImpl( int aIndex, Color aColor, String aName, List<Channel> aChannels )
    {
      this.index = aIndex;
      this.channels = aChannels;
      this.color = aColor;
      this.name = aName;
    }

    // METHODS

    public void addChannel( Channel aChannel )
    {
      this.channels.add( aChannel );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( ChannelGroup aGroup )
    {
      return this.index - aGroup.getIndex();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( Object aObject )
    {
      if ( this == aObject )
      {
        return true;
      }
      if ( aObject == null || getClass() != aObject.getClass() )
      {
        return false;
      }

      ChannelGroupImpl other = ( ChannelGroupImpl )aObject;
      if ( this.index != other.index )
      {
        return false;
      }
      if ( this.name == null )
      {
        if ( other.name != null )
        {
          return false;
        }
      }
      else if ( !this.name.equals( other.name ) )
      {
        return false;
      }
      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Channel[] getChannels()
    {
      return this.channels.toArray( new Channel[this.channels.size()] );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Color getColor()
    {
      return this.color;
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
    public String getName()
    {
      return this.name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + this.index;
      result = prime * result + ( ( this.name == null ) ? 0 : this.name.hashCode() );
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColor( Color aColor )
    {
      this.color = aColor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setName( String aName )
    {
      if ( aName == null )
      {
        throw new IllegalArgumentException( "Name cannot be null!" );
      }
      this.name = aName;
    }

    /**
     * @param aChannel
     *          the channel to remove from this group, cannot be
     *          <code>null</code>.
     */
    void remove( ChannelImpl aChannel )
    {
      this.channels.remove( aChannel );
    }
  }

  // VARIABLES

  private final Set<Integer> channelIndices;
  private Color color;
  private String name;
  private int index;
  private int channelCount;

  // CONSTRUCTORS

  /**
   * Creates a new ChannelGroupBuilder instance.
   */
  ChannelGroupBuilder()
  {
    this.index = -1;
    this.color = null;
    this.name = null;
    this.channelCount = -1;
    this.channelIndices = new LinkedHashSet<Integer>();
  }

  // METHODS

  public ChannelGroupBuilder addChannel( Channel... aChannels )
  {
    for ( Channel channel : aChannels )
    {
      this.channelIndices.add( channel.getIndex() );
    }
    return this;
  }

  public ChannelGroupBuilder addChannel( int... aChannelIdices )
  {
    for ( int channelIdx : aChannelIdices )
    {
      this.channelIndices.add( channelIdx );
    }
    return this;
  }

  public ChannelGroupBuilder setChannelCount( int aCount )
  {
    this.channelCount = aCount;
    return this;
  }

  public ChannelGroupBuilder setColor( Color aColor )
  {
    this.color = aColor;
    return this;
  }

  public ChannelGroupBuilder setIndex( int aIndex )
  {
    this.index = aIndex;
    return this;
  }

  public ChannelGroupBuilder setName( String aName )
  {
    this.name = aName;
    return this;
  }

  ChannelGroupImpl build( AcquisitionDataBuilder aDataBuilder )
  {
    if ( this.index < 0 )
    {
      throw new IllegalArgumentException( "Invalid channel index: " + this.index );
    }
    if ( this.channelIndices.isEmpty() )
    {
      throw new IllegalArgumentException( "No channels are assigned to channel group " + this.index + "!" );
    }
    if ( this.name == null )
    {
      this.name = String.format( "Group %d", index );
    }
    if ( this.channelCount < 0 )
    {
      this.channelCount = this.channelIndices.size();
    }

    List<Channel> channels = new ArrayList<Channel>();
    ChannelGroupImpl group = new ChannelGroupImpl( this.index, this.color, this.name, channels );

    for ( Integer channelIdx : this.channelIndices )
    {
      ChannelImpl channel = aDataBuilder.getChannel( channelIdx );
      if ( channel == null )
      {
        // Lazily create one...
        channel = aDataBuilder.add( aDataBuilder.createChannel().setIndex( channelIdx ).build( aDataBuilder ) );
      }
      channel.setChannelGroup( group );
      channels.add( channel );
    }

    return group;
  }
}
