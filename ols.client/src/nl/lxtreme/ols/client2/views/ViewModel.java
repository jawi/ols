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
package nl.lxtreme.ols.client2.views;


import javax.swing.event.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Represents the model for all views.
 */
public class ViewModel
{
  // VARIABLES

  private final EventListenerList listeners;
  private final Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ViewModel} instance.
   * 
   * @param aSession
   *          the {@link Session} to use, cannot be <code>null</code>.
   */
  public ViewModel( Session aSession )
  {
    this.session = aSession;

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
    return this.session.getAcquiredData();
  }

  /**
   * Returns the current session.
   * 
   * @return the session, never <code>null</code>.
   */
  public Session getSession()
  {
    return this.session;
  }

  /**
   * @return a title for this model, never <code>null</code>.
   */
  public String getTitle()
  {
    return String.format( "Session #%d", this.session.getId() );
  }

  /**
   * @return <code>true</code> if timing data (for waveform views) is present,
   *         <code>false</code> otherwise.
   */
  public boolean hasTimingData()
  {
    return getData().hasTimingData();
  }

  /**
   * Initializes this model.
   */
  public void initialize()
  {
    // TODO
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

  /**
   * Zooms the current view in such way that all data is visible.
   */
  public void zoomAll()
  {
    // Nop
  }

  /**
   * Zooms in.
   */
  public void zoomIn()
  {
    // Nop
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    // Nop
  }

  /**
   * Zooms out.
   */
  public void zoomOut()
  {
    // Nop
  }
}
