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
package nl.lxtreme.ols.client.project.impl;


import java.beans.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * Provides a implementation of {@link DataSet}.
 */
public class DataSetImpl implements PropertyChangeListener, DataSet, ProjectProperties
{
  // VARIABLES

  private final PropertyChangeSupport propertyChangeSupport;
  private final AcquisitionResult capturedData;
  private final Cursor[] cursors;

  private final Channel[] channels;
  private boolean cursorsEnabled;

  // CONSTRUCTORS

  /**
   * Creates a new DataSetImpl instance.
   * 
   * @param aCapturedData
   *          the captured data of this data set;
   * @param aOld
   *          the old data set to template.
   */
  public DataSetImpl( final AcquisitionResult aCapturedData, final DataSet aOld, final boolean aRetainAnnotations )
  {
    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.capturedData = aCapturedData;
    this.cursorsEnabled = aOld.isCursorsEnabled();
    this.channels = createChannels( aCapturedData.getChannels(), aCapturedData.getEnabledChannels(), aRetainAnnotations,
        aOld.getChannels() );
    this.cursors = createCursors( Ols.MAX_CURSORS, aOld.getCursors() );
  }

  /**
   * Creates a new {@link DataSetImpl} instance.
   */
  DataSetImpl()
  {
    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.capturedData = null;
    this.cursors = createCursors( Ols.MAX_CURSORS );
    this.channels = createChannels( Ols.MAX_CHANNELS, 0xFFFFFFFF,
        false /* aRetainAnnotations */ );
    this.cursorsEnabled = true;
  }

  // METHODS

  /**
   * Adds the given listener to the list of property change listeners.
   * 
   * @param aListener
   *          a property change listener, cannot be <code>null</code>.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.addPropertyChangeListener( aListener );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult getCapturedData()
  {
    return this.capturedData;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Channel getChannel( final int aIndex )
  {
    Channel channel = this.channels[aIndex];
    if ( channel == null )
    {
      channel = new ChannelImpl( aIndex );
      this.channels[aIndex] = channel;
    }
    return channel;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Channel[] getChannels()
  {
    List<Channel> result = new ArrayList<Channel>();
    // Issue #244 - only return non-null channels...
    for ( Channel chan : this.channels )
    {
      if ( chan != null )
      {
        result.add( chan );
      }
    }
    return result.toArray( size -> new Channel[size] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor getCursor( final int aIndex )
  {
    return this.cursors[aIndex];
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor[] getCursors()
  {
    return Arrays.copyOf( this.cursors, this.cursors.length );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isCursorsEnabled()
  {
    return this.cursorsEnabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    this.propertyChangeSupport.firePropertyChange( aEvent );
  }

  /**
   * Removes the given listener from the list of property change listeners.
   * 
   * @param aListener
   *          a property change listener, cannot be <code>null</code>.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.removePropertyChangeListener( aListener );
  }

  /**
   * @see nl.lxtreme.ols.api.data.project.Project#setCursorsEnabled(boolean)
   */
  @Override
  public void setCursorsEnabled( final boolean aEnabled )
  {
    final boolean old = this.cursorsEnabled;
    this.cursorsEnabled = aEnabled;

    this.propertyChangeSupport.firePropertyChange( PROPERTY_CURSORS_ENABLED, old, aEnabled );
  }

  /**
   * Trims the channels to the same number as the captured data, using the
   * channel information of the given data set as template.
   * 
   * @param aLabels
   *          the channel labels to merge, can be <code>null</code>.
   */
  final void mergeChannelLabels( final List<String> aLabels )
  {
    if ( aLabels == null )
    {
      return;
    }

    int channelCount = Math.min( aLabels.size(), this.channels.length );
    for ( int i = 0; i < channelCount; i++ )
    {
      Channel channel = this.channels[i];
      // Channels might be null, especially when importing projects from older
      // versions of the client...
      if ( channel != null )
      {
        channel.setLabel( aLabels.get( i ) );
      }
    }
  }

  /**
   * Creates an array of a given number of channels.
   * 
   * @param aCount
   *          the number of channels to create, >= 0.
   * @return an array with channels, never <code>null</code>.
   */
  private Channel[] createChannels( final int aCount, final int aMask, final boolean aRetainAnnotations,
      final Channel... aInitialValues )
  {
    final int chCount = ( this.capturedData == null ) ? aCount : this.capturedData.getChannels();

    Channel[] result = new Channel[aCount];
    for ( int i = 0, j = 0; ( j < aCount ) && ( i < Ols.MAX_CHANNELS ); i++ )
    {
      final int mask = ( 1 << i );
      // Issue #99: demultiplex the channels to the right group...
      if ( ( aMask & mask ) == 0 )
      {
        continue;
      }

      ChannelImpl channel;
      // Only use an initial version of channel if the channel indexes match...
      if ( ( j < aInitialValues.length ) && ( j < chCount ) && ( aInitialValues[j] != null )
          && ( aInitialValues[j].getIndex() == i ) )
      {
        channel = new ChannelImpl( aInitialValues[j], aRetainAnnotations );
      }
      else
      {
        channel = new ChannelImpl( i );
      }
      channel.addPropertyChangeListener( this );

      result[j++] = channel;
    }
    return result;
  }

  /**
   * Creates an array of a given number of channels.
   * 
   * @param aCount
   *          the number of channels to create, >= 0.
   * @return an array with channels, never <code>null</code>.
   */
  private Cursor[] createCursors( final int aCount, final Cursor... aInitialValues )
  {
    final long absLen = this.capturedData == null ? 0L : this.capturedData.getAbsoluteLength();

    Cursor[] result = new Cursor[aCount];
    for ( int i = 0; i < aCount; i++ )
    {
      CursorImpl cursor;
      if ( i < aInitialValues.length )
      {
        cursor = new CursorImpl( aInitialValues[i], absLen );
      }
      else
      {
        cursor = new CursorImpl( i );
      }
      cursor.addPropertyChangeListener( this );

      result[i] = cursor;
    }
    return result;
  }
}
