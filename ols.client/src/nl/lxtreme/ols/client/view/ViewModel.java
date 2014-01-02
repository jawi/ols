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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.view;


import javax.swing.event.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Represents the model for all views.
 */
public class ViewModel
{
  // VARIABLES

  private final EventListenerList listeners;
  private final AcquisitionData data;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ViewModel} instance.
   * 
   * @param aData
   *          the {@link AcquisitionData} to use, cannot be <code>null</code>.
   */
  public ViewModel( AcquisitionData aData )
  {
    this.data = aData;

    this.listeners = new EventListenerList();
  }

  // METHODS

  /**
   * Adds a given listener to the list of marker change listeners.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addMarkerChangeListener( IMarkerChangeListener aListener )
  {
    this.listeners.add( IMarkerChangeListener.class, aListener );
  }

  /**
   * Returns the actual acquisition data.
   * 
   * @return the data, never <code>null</code>.
   */
  public AcquisitionData getData()
  {
    return this.data;
  }

  /**
   * @return <code>true</code> if timing data (for waveform views) is present,
   *         <code>false</code> otherwise.
   */
  public boolean hasTimingData()
  {
    return this.data.hasTimingData();
  }

  /**
   * Initializes this model.
   */
  public void initialize()
  {
    // TODO
  }

  public void notifyMarkerAdded( IMarker aMarker )
  {
    for ( IMarkerChangeListener listener : this.listeners.getListeners( IMarkerChangeListener.class ) )
    {
      listener.markerAdded( aMarker );
    }
  }

  public void notifyMarkerChanged( String aPropertyName, IMarker aOldMarker, IMarker aNewMarker )
  {
    for ( IMarkerChangeListener listener : this.listeners.getListeners( IMarkerChangeListener.class ) )
    {
      listener.markerChanged( aPropertyName, aOldMarker, aNewMarker );
    }
  }

  public void notifyMarkerRemoved( IMarker aMarker )
  {
    for ( IMarkerChangeListener listener : this.listeners.getListeners( IMarkerChangeListener.class ) )
    {
      listener.markerRemoved( aMarker );
    }
  }

  public void notifyMarkersInvisible()
  {
    for ( IMarkerChangeListener listener : this.listeners.getListeners( IMarkerChangeListener.class ) )
    {
      listener.markersInvisible();
    }
  }

  public void notifyMarkersVisible()
  {
    for ( IMarkerChangeListener listener : this.listeners.getListeners( IMarkerChangeListener.class ) )
    {
      listener.markersVisible();
    }
  }

  /**
   * Removes a given listener from the list of marker change listeners.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeMarkerChangeListener( IMarkerChangeListener aListener )
  {
    this.listeners.remove( IMarkerChangeListener.class, aListener );
  }
}
