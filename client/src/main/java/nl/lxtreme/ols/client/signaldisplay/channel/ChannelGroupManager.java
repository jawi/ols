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

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.IChannelChangeListener.*;


/**
 * Manages all channel groups.
 */
public final class ChannelGroupManager implements IDataModelChangeListener
{
  // CONSTANTS

  public static final int MAX_CHANNEL_GROUPS = Channel.MAX_CHANNELS;

  // VARIABLES

  private final List<ChannelGroup> channelGroups;
  private final EventListenerList eventListeners;

  private Channel[] channels;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelGroupManager} instance.
   */
  public ChannelGroupManager()
  {
    this.channelGroups = new ArrayList<ChannelGroup>();
    this.eventListeners = new EventListenerList();

    this.channels = new Channel[0];
  }

  // METHODS

  /**
   * Initializes the channels.
   * 
   * @param aCount
   *          the number of channels to initialize, >= 0.
   */
  private static Channel[] createChannels( final int aCount )
  {
    Channel[] channels = new Channel[aCount];
    for ( int i = 0; i < channels.length; i++ )
    {
      channels[i] = new Channel( i );
    }
    return channels;
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
  public void addChannel( final ChannelGroup aChannelGroup, final Channel aChannel )
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
  public ChannelGroup addChannelGroup( final String aName )
  {
    final Channel firstAvailableChannel = getFirstUnassignedChannel();
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
   * Returns whether or not a new channel group can be added.
   * 
   * @return <code>true</code> if a new channel group can be added,
   *         <code>false</code> otherwise.
   */
  public boolean canAddChannelGroup()
  {
    return !getUnassignedChannels().isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final AcquisitionResult aCapturedData )
  {
    this.channelGroups.clear();
    this.channels = createChannels( aCapturedData.getChannels() );

    // Reset channel groups so they align with the given data model...
    final int maxI = this.channels.length / 8;
    final int maxJ = 8;
    for ( int i = 0; i < maxI; i++ )
    {
      ChannelGroup channelGroup = addChannelGroup( "Group " + ( i + 1 ) );
      // channelGroup.setVisible( ( i % 2 ) == 0 );

      for ( int j = 0; j < maxJ; j++ )
      {
        channelGroup.addChannel( this.channels[( i * maxJ ) + j] );
      }
    }

    fireChannelGroupStructureChangeEvent( getAssignedChannels() );
  }

  /**
   * @param aEvent
   */
  public void fireChannelChangeEvent( final ChannelChangeEvent aEvent )
  {
    final IChannelChangeListener[] listeners = this.eventListeners.getListeners( IChannelChangeListener.class );
    for ( IChannelChangeListener listener : listeners )
    {
      listener.channelChanged( aEvent );
    }
  }

  /**
   * @param aEvent
   */
  public void fireChannelMoveEvent( final ChannelMoveEvent aEvent )
  {
    final IChannelChangeListener[] listeners = this.eventListeners.getListeners( IChannelChangeListener.class );
    for ( IChannelChangeListener listener : listeners )
    {
      listener.channelMoved( aEvent );
    }
  }

  /**
   * Returns all channels used in this signal diagram model.
   * 
   * @return an array of channels, never <code>null</code>.
   */
  public Channel[] getAllChannels()
  {
    return this.channels;
  }

  /**
   * Returns a sorted set of all assigned (not available) channels.
   * 
   * @return a sorted set of all assigned channels, never <code>null</code>.
   */
  public SortedSet<Channel> getAssignedChannels()
  {
    SortedSet<Channel> channelIndexes = new TreeSet<Channel>();

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
  public Channel getChannel( final int aIndex )
  {
    Channel result = null;
    Iterator<ChannelGroup> channelGroupIter = this.channelGroups.iterator();

    while ( channelGroupIter.hasNext() && ( result == null ) )
    {
      ChannelGroup cg = channelGroupIter.next();
      result = cg.getChannel( aIndex );
    }

    return result;
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
    Iterator<ChannelGroup> channelGroupIter = this.channelGroups.iterator();

    while ( channelGroupIter.hasNext() && ( result == null ) )
    {
      ChannelGroup cg = channelGroupIter.next();
      result = cg.getChannelByIndex( aIndex );
    }

    return result;
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

    for ( ChannelGroup cg : this.channelGroups )
    {
      if ( aName.equals( cg.getName() ) )
      {
        return cg;
      }
    }
    return null;
  }

  /**
   * Returns all current channel groups.
   * 
   * @return an array of channel groups, never <code>null</code>.
   */
  public ChannelGroup[] getChannelGroups()
  {
    final int size = this.channelGroups.size();
    return this.channelGroups.toArray( new ChannelGroup[size] );
  }

  /**
   * Returns a sorted set of all unassigned (= available) channels.
   * 
   * @return a sorted set of unassigned channels, never <code>null</code>.
   */
  public SortedSet<Channel> getUnassignedChannels()
  {
    SortedSet<Channel> channelIndexes = new TreeSet<Channel>();
    channelIndexes.addAll( Arrays.asList( this.channels ) );

    for ( ChannelGroup cg : this.channelGroups )
    {
      channelIndexes.removeAll( Arrays.asList( cg.getChannels() ) );
    }

    return channelIndexes;
  }

  /**
   * Returns the number of visible channels.
   * 
   * @return a channel count, >= 0.
   */
  public int getVisibleChannelCount()
  {
    int count = 0;
    for ( ChannelGroup cg : this.channelGroups )
    {
      if ( cg.isVisible() )
      {
        count += cg.getChannels().length;
      }
    }

    return count;
  }

  /**
   * Moves a channel with a given index to a new index.
   * 
   * @param aMovedChannel
   *          the channel to move;
   * @param aInsertIndex
   *          the insertion index of the channel to move.
   */
  public void moveChannel( final Channel aMovedChannel, final Channel aInsertChannel )
  {
    if ( ( aMovedChannel != null ) && ( aInsertChannel != null ) )
    {
      final ChannelGroup cg = aInsertChannel.getChannelGroup();
      cg.moveChannel( aMovedChannel, aInsertChannel.getVirtualIndex() );
    }
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
  public void removeChannel( final ChannelGroup aChannelGroup, final Channel aChannel )
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
   * Removes the channel group with the given name.
   * 
   * @param aName
   *          the name of the channel group to remove, cannot be
   *          <code>null</code> or empty.
   * @throws IllegalArgumentException
   *           in case the given name was <code>null</code> or empty.
   */
  public void removeChannelGroup( final String aName )
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
   * @param aEvent
   */
  final void fireChannelGroupStructureChangeEvent( final Collection<Channel> aEvent )
  {
    final IChannelChangeListener[] listeners = this.eventListeners.getListeners( IChannelChangeListener.class );
    for ( IChannelChangeListener listener : listeners )
    {
      listener.channelGroupStructureChanged( aEvent );
    }
  }

  /**
   * Returns the first available channel for a (new) channel group.
   * 
   * @return a channel, or <code>null</code> if no channels are available.
   */
  private Channel getFirstUnassignedChannel()
  {
    SortedSet<Channel> channels = getUnassignedChannels();

    // Any channels left?
    if ( ( channels == null ) || channels.isEmpty() )
    {
      return null;
    }

    return channels.first();
  }
}
