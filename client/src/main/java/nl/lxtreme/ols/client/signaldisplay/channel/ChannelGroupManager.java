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


import java.util.*;

import javax.swing.event.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.IChannelChangeListener.ChannelChangeEvent;
import nl.lxtreme.ols.client.signaldisplay.IChannelChangeListener.ChannelMoveEvent;
import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Manages all channel groups.
 */
public final class ChannelGroupManager implements IDataModelChangeListener
{
  // VARIABLES

  private final List<ChannelGroup> channelGroups;
  private final EventListenerList eventListeners;

  private GroupableChannel[] channels;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelGroupManager} instance.
   */
  public ChannelGroupManager()
  {
    this.channelGroups = new ArrayList<ChannelGroup>();
    this.eventListeners = new EventListenerList();

    this.channels = new GroupableChannel[0];
  }

  // METHODS

  /**
   * Adds a channel change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addChannelChangeListener( final IChannelChangeListener aListener )
  {
    this.eventListeners.add( IChannelChangeListener.class, aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final DataSet aCapturedData )
  {
    // Make sure only a single thread at a time modifies us...
    synchronized ( this.channelGroups )
    {
      this.channelGroups.clear();
      this.channels = createChannels( aCapturedData.getChannels() );

      // Reset channel groups so they align with the given data model...
      final int groupCount = Math.max( 1, ( int )Math.ceil( this.channels.length / ( double )Ols.CHANNELS_PER_BLOCK ) );
      final int channelsPerGroup = ( int )Math.ceil( this.channels.length / ( double )groupCount );

      for ( int g = 0; g < groupCount; g++ )
      {
        final ChannelGroup channelGroup = addChannelGroup( "Group " + ( g + 1 ) );

        for ( int c = 0; c < channelsPerGroup; c++ )
        {
          final int channelIdx = ( g * channelsPerGroup ) + c;
          addChannel( channelGroup, this.channels[channelIdx] );
        }
      }
    }

    fireChannelGroupStructureChangeEvent( getAssignedChannels() );
  }

  /**
   * Returns all channels used in this signal diagram model.
   * 
   * @return a collection of all channels, never <code>null</code>.
   */
  public Collection<Channel> getAllChannels()
  {
    List<Channel> result = new ArrayList<Channel>();
    if ( this.channels != null )
    {
      Collections.addAll( result, this.channels );
    }
    return result;
  }

  /**
   * Returns a sorted set of all assigned (not available) channels.
   * 
   * @return a sorted set of all assigned channels, never <code>null</code>.
   */
  public SortedSet<GroupableChannel> getAssignedChannels()
  {
    SortedSet<GroupableChannel> channelIndexes = new TreeSet<GroupableChannel>();

    for ( ChannelGroup cg : this.channelGroups )
    {
      channelIndexes.addAll( Arrays.asList( cg.getChannels() ) );
    }

    return channelIndexes;
  }

  /**
   * Returns the channel with a given index.
   * 
   * @param aIndex
   *          the index of the channel to return.
   * @return the channel with the given index, or <code>null</code> if no such
   *         channel was found.
   */
  public Channel getChannelByIndex( final int aIndex )
  {
    Channel result = null;

    synchronized ( this.channelGroups )
    {
      Iterator<ChannelGroup> channelGroupIter = this.channelGroups.iterator();
      while ( channelGroupIter.hasNext() && ( result == null ) )
      {
        ChannelGroup cg = channelGroupIter.next();
        result = cg.getChannelByIndex( aIndex );
      }
    }

    return result;
  }

  /**
   * Returns the channel group the given channel belongs to.
   * 
   * @param aChannel
   *          the channel of which to return the channel group, cannot be
   *          <code>null</code>.
   * @return a channel group, never <code>null</code>.
   */
  public ChannelGroup getChannelGroup( final Channel aChannel )
  {
    GroupableChannel channel = asGroupableChannel( aChannel );
    if ( channel != null )
    {
      return channel.getChannelGroup();
    }
    return null;
  }

  /**
   * @param aMovedChannel
   * @return
   */
  public ChannelGroup getChannelGroup( final SignalElement aSignalElement )
  {
    if ( aSignalElement.isDigitalSignal() )
    {
      return asGroupableChannel( aSignalElement.getChannel() ).getChannelGroup();
    }
    else
    {
      return aSignalElement.getChannelGroup();
    }
  }

  /**
   * Returns the channel group with a given name.
   * 
   * @param aName
   *          the name of the channel group to return, cannot be
   *          <code>null</code> or empty.
   * @return the channel group with the given name, or <code>null</code> if no
   *         such channel group exists.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty.
   */
  public ChannelGroup getChannelGroupByName( final String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    synchronized ( this.channelGroups )
    {
      for ( ChannelGroup cg : this.channelGroups )
      {
        if ( aName.equals( cg.getName() ) )
        {
          return cg;
        }
      }
      return null;
    }
  }

  /**
   * Returns all current channel groups.
   * 
   * @return an array of channel groups, never <code>null</code>.
   */
  public ChannelGroup[] getChannelGroups()
  {
    synchronized ( this.channelGroups )
    {
      final int size = this.channelGroups.size();
      return this.channelGroups.toArray( new ChannelGroup[size] );
    }
  }

  /**
   * Moves a channel with a given index to a new index.
   * 
   * @param aMovedChannel
   *          the channel to move;
   * @param aInsertElement
   *          the channel before which the moved channel is to be inserted.
   */
  public void moveChannel( final Channel aMovedChannel, final SignalElement aInsertElement )
  {
    if ( ( aMovedChannel != null ) && ( aInsertElement != null ) )
    {
      final GroupableChannel movedChannel = asGroupableChannel( aMovedChannel );

      final ChannelGroup oldCG = movedChannel.getChannelGroup();
      final ChannelGroup cg = getChannelGroup( aInsertElement );

      int oldIndex = movedChannel.getVirtualIndex();
      int newIndex = 0;
      int offset = ( oldCG.getIndex() - cg.getIndex() );

      if ( aInsertElement.isDigitalSignal() )
      {
        newIndex = asGroupableChannel( aInsertElement.getChannel() ).getVirtualIndex();
      }
      else if ( aInsertElement.isSignalGroup() )
      {
        newIndex = 0; //
        offset = 0;
      }
      else
      {
        newIndex = getChannelGroup( aInsertElement ).getChannelCount();
      }

      cg.moveChannel( movedChannel, newIndex + offset );

      fireChannelMoveEvent( new ChannelMoveEvent( movedChannel, oldCG, oldIndex ) );
    }
  }

  /**
   * Removes a channel change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeChannelChangeListener( final IChannelChangeListener aListener )
  {
    this.eventListeners.remove( IChannelChangeListener.class, aListener );
  }

  /**
   * Fires a {@link ChannelChangeEvent} to all interested listeners.
   * 
   * @param aEvent
   *          the event to fire,cannot be <code>null</code>.
   */
  final void fireChannelChangeEvent( final ChannelChangeEvent aEvent )
  {
    final IChannelChangeListener[] listeners = this.eventListeners.getListeners( IChannelChangeListener.class );
    for ( IChannelChangeListener listener : listeners )
    {
      listener.channelChanged( aEvent );
    }
  }

  /**
   * Fires a "channelgroup structure changed"-event to all interested listeners.
   * 
   * @param aEvent
   *          the event to fire,cannot be <code>null</code>.
   */
  final void fireChannelGroupStructureChangeEvent( final Collection<GroupableChannel> aEvent )
  {
    final IChannelChangeListener[] listeners = this.eventListeners.getListeners( IChannelChangeListener.class );
    for ( IChannelChangeListener listener : listeners )
    {
      listener.channelGroupStructureChanged( aEvent );
    }
  }

  /**
   * Fires a {@link ChannelMoveEvent} to all interested listeners.
   * 
   * @param aEvent
   *          the event to fire,cannot be <code>null</code>.
   */
  final void fireChannelMoveEvent( final ChannelMoveEvent aEvent )
  {
    final IChannelChangeListener[] listeners = this.eventListeners.getListeners( IChannelChangeListener.class );
    for ( IChannelChangeListener listener : listeners )
    {
      listener.channelMoved( aEvent );
    }
  }

  /**
   * Adds a given channel to the given channel group.
   * <p>
   * If the given channel group already contains the given channel, then this
   * method is effectively a no-op.
   * </p>
   * 
   * @param aChannelGroup
   *          the channel group to add the channel to, cannot be
   *          <code>null</code>;
   * @param aChannel
   *          the channel to add to the channel group, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case one of the given parameters was <code>null</code>.
   */
  protected void addChannel( final ChannelGroup aChannelGroup, final GroupableChannel aChannel )
  {
    if ( aChannelGroup == null )
    {
      throw new IllegalArgumentException( "ChannelGroup cannot be null!" );
    }
    if ( aChannel == null )
    {
      throw new IllegalArgumentException( "Channel cannot be null!" );
    }

    if ( aChannelGroup.hasChannel( aChannel ) )
    {
      // Nothing to do; we're done...
      return;
    }

    // Keep a reference to the former channel group...
    final ChannelGroup oldCG = aChannel.getChannelGroup();
    // This will automatically remove the given channel from its former channel
    // group...
    aChannelGroup.addChannel( aChannel );
    // When there are no more channels left in this channel group, remove it...
    if ( ( oldCG != null ) && !oldCG.hasChannels() )
    {
      this.channelGroups.remove( oldCG );
    }
  }

  /**
   * Adds a new channel group to this manager.
   * 
   * @param aName
   *          the name of the new channel group, cannot be <code>null</code> or
   *          empty.
   * @return the newly added channel group, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty;
   * @throws IllegalStateException
   *           in case no channels are available for the new channel group.
   */
  protected ChannelGroup addChannelGroup( final String aName )
  {
    final GroupableChannel firstAvailableChannel = getFirstUnassignedChannel();
    if ( firstAvailableChannel == null )
    {
      throw new IllegalStateException( "No channels left!" );
    }

    ChannelGroup result = new ChannelGroup( this.channelGroups.size(), aName );
    // For convenience, add the first available channel to this group...
    result.addChannel( firstAvailableChannel );

    this.channelGroups.add( result );

    return result;
  }

  /**
   * Returns a sorted set of all unassigned (= available) channels.
   * 
   * @return a sorted set of unassigned channels, never <code>null</code>.
   */
  protected SortedSet<GroupableChannel> getUnassignedChannels()
  {
    SortedSet<GroupableChannel> channelIndexes = new TreeSet<GroupableChannel>();
    channelIndexes.addAll( Arrays.asList( this.channels ) );

    for ( ChannelGroup cg : this.channelGroups )
    {
      channelIndexes.removeAll( Arrays.asList( cg.getChannels() ) );
    }

    return channelIndexes;
  }

  /**
   * Removes a channel from a given channel group.
   * 
   * @param aChannelGroup
   *          the channel group to remove the channel from, cannot be
   *          <code>null</code>;
   * @param aChannel
   *          the channel to remove, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case one of the given parameters was <code>null</code>.
   */
  protected void removeChannel( final ChannelGroup aChannelGroup, final GroupableChannel aChannel )
  {
    if ( aChannelGroup == null )
    {
      throw new IllegalArgumentException( "ChannelGroup cannot be null!" );
    }
    if ( aChannel == null )
    {
      throw new IllegalArgumentException( "Channel cannot be null!" );
    }

    aChannelGroup.removeChannel( aChannel );
  }

  /**
   * Removes the channel group with the given name.
   * 
   * @param aName
   *          the name of the channel group to remove, cannot be
   *          <code>null</code> or empty.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty.
   */
  protected void removeChannelGroup( final String aName )
  {
    if ( ( aName == null ) || aName.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    ChannelGroup cg = getChannelGroupByName( aName );
    if ( cg != null )
    {
      this.channelGroups.remove( cg );
    }
  }

  /**
   * @param aChannel
   * @return
   */
  private GroupableChannel asGroupableChannel( final Channel aChannel )
  {
    if ( aChannel instanceof GroupableChannel )
    {
      return ( GroupableChannel )aChannel;
    }
    throw new RuntimeException( "TODO wrap me!" );
  }

  /**
   * Wraps the given array of channels into groupable channels.
   * 
   * @param aChannels
   *          the channels to wrap, cannot be <code>null</code>.
   * @return an array (of the exact same size as the given array) with groupable
   *         channels.
   */
  private GroupableChannel[] createChannels( final Channel[] aChannels )
  {
    GroupableChannel[] result = new GroupableChannel[aChannels.length];
    for ( int i = 0; i < result.length; i++ )
    {
      result[i] = new GroupableChannel( this, aChannels[i] );
    }
    return result;
  }

  /**
   * Returns the first available channel for a (new) channel group.
   * 
   * @return a channel, or <code>null</code> if no channels are available.
   */
  private GroupableChannel getFirstUnassignedChannel()
  {
    SortedSet<GroupableChannel> channels = getUnassignedChannels();

    // Any channels left?
    if ( ( channels == null ) || channels.isEmpty() )
    {
      return null;
    }

    return channels.first();
  }
}
