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
public class DataSetImpl implements DataSet, ProjectProperties
{
  // VARIABLES

  private final PropertyChangeSupport propertyChangeSupport;
  private final AcquisitionResult capturedData;
  private final Cursor[] cursors;

  private Channel[] channels;
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
  public DataSetImpl( final AcquisitionResult aCapturedData, final DataSet aOld )
  {
    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.capturedData = aCapturedData;
    this.cursors = createCursors( Ols.MAX_CURSORS, aOld.getCursors() );
    this.channels = createChannels( aCapturedData.getChannels(), aOld.getChannels() );
    this.cursorsEnabled = aOld.isCursorsEnabled();
  }

  /**
   * Creates a new {@link DataSetImpl} instance.
   */
  DataSetImpl()
  {
    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.capturedData = null;
    this.cursors = createCursors( Ols.MAX_CURSORS );
    this.channels = createChannels( Ols.MAX_CHANNELS );
    this.cursorsEnabled = false;
  }

  // METHODS

  /**
   * Creates an array of a given number of channels.
   * 
   * @param aCount
   *          the number of channels to create, >= 0.
   * @return an array with channels, never <code>null</code>.
   */
  private static Channel[] createChannels( final int aCount, final Channel... aInitialValues )
  {
    Channel[] result = new Channel[aCount];
    for ( int i = 0; i < aCount; i++ )
    {
      if ( i < aInitialValues.length )
      {
        result[i] = new ChannelImpl( aInitialValues[i] );
      }
      else
      {
        result[i] = new ChannelImpl( i );
      }
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
  private static Cursor[] createCursors( final int aCount, final Cursor... aInitialValues )
  {
    Cursor[] result = new Cursor[aCount];
    for ( int i = 0; i < aCount; i++ )
    {
      if ( i < aInitialValues.length )
      {
        result[i] = new CursorImpl( aInitialValues[i] );
      }
      else
      {
        result[i] = new CursorImpl( i );
      }
    }
    return result;
  }

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
    return Arrays.copyOf( this.channels, this.channels.length );
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
   */
  final void mergeChannelData( final DataSetImpl aDataSet )
  {
    int channelCount = aDataSet.getChannels().length;
    this.channels = createChannels( channelCount, aDataSet.getChannels() );
  }
}
