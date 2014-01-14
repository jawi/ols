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


import java.util.concurrent.atomic.*;

import nl.lxtreme.ols.client2.views.MeasurementInfoBuilder.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Represents the model for all views.
 */
public class ViewModel
{
  // VARIABLES

  private final Session session;
  private final AtomicReference<Cursor> selectedCursorRef;
  private final AtomicReference<MeasurementInfo> measurementInfoRef;
  private final AtomicBoolean measurementFrozen;

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
    this.selectedCursorRef = new AtomicReference<Cursor>();
    this.measurementInfoRef = new AtomicReference<MeasurementInfo>();
    this.measurementFrozen = new AtomicBoolean( false );
  }

  // METHODS

  /**
   * @see AcquisitionData#areCursorsVisible()
   */
  public final boolean areCursorsVisible()
  {
    return getData().areCursorsVisible();
  }

  /**
   * @return the annotation data, never <code>null</code>.
   */
  public final AnnotationData getAnnotations()
  {
    return this.session.getAnnotationData();
  }

  /**
   * @return the array with all cursors, never <code>null</code>.
   */
  public final Cursor[] getCursors()
  {
    return getData().getCursors();
  }

  /**
   * Returns the actual acquisition data.
   * 
   * @return the data, never <code>null</code>.
   */
  public final AcquisitionData getData()
  {
    return this.session.getAcquiredData();
  }

  /**
   * @return the last known measurement information, can be <code>null</code>.
   */
  public final MeasurementInfo getMeasurementInfo()
  {
    return this.measurementInfoRef.get();
  }

  /**
   * @return the next available cursor, or <code>null</code> in case no cursor
   *         is available.
   */
  public final Cursor getNextAvailableCursor()
  {
    Cursor[] cursors = getCursors();
    int i = cursors.length - 1;
    for ( ; i >= 0; i-- )
    {
      if ( cursors[i].isDefined() )
      {
        // Highest defined cursor found...
        break;
      }
    }
    if ( i < 0 || i == cursors.length - 1 )
    {
      return null;
    }

    return cursors[i + 1];
  }

  /**
   * Returns the selected cursor.
   * 
   * @return a selected cursor, can be <code>null</code>.
   */
  public final Cursor getSelectedCursor()
  {
    return this.selectedCursorRef.get();
  }

  /**
   * Returns the current session.
   * 
   * @return the session, never <code>null</code>.
   */
  public final Session getSession()
  {
    return this.session;
  }

  /**
   * @return the name for the contained session, never <code>null</code>.
   */
  public final String getSessionName()
  {
    String name = this.session.getName();
    if ( name == null )
    {
      name = String.format( "Session #%d", this.session.getId() );
    }
    return name;
  }

  /**
   * @see AcquisitionData#hasTimingData()
   */
  public final boolean hasTimingData()
  {
    return getData().hasTimingData();
  }

  /**
   * Initializes this model.
   */
  public void initialize()
  {
    // Nop
  }

  /**
   * @return <code>true</code> if the current measurement is frozen, i.e, no
   *         longer is updated by mouse movements, <code>false</code> otherwise.
   */
  public final boolean isMeasurementFrozen()
  {
    return this.measurementFrozen.get();
  }

  /**
   * Sets whether or not the measurement mode is frozen.
   * 
   * @param aFrozen
   *          <code>true</code> if the measurement mode is frozen,
   *          <code>false</code> otherwise.
   */
  public final void setMeasurementFrozen( boolean aFrozen )
  {
    boolean old;
    do
    {
      old = this.measurementFrozen.get();
    }
    while ( !this.measurementFrozen.compareAndSet( old, aFrozen ) );
  }

  /**
   * Sets the measurement information to the one given.
   * 
   * @param aMeasurementInfo
   *          the measurement information to set, can be <code>null</code>.
   */
  public final void setMeasurementInfo( MeasurementInfo aMeasurementInfo )
  {
    MeasurementInfo old;
    do
    {
      old = this.measurementInfoRef.get();
    }
    while ( !this.measurementInfoRef.compareAndSet( old, aMeasurementInfo ) );
  }

  /**
   * Sets the selected cursor to the one given.
   * 
   * @param aCursor
   *          the selected cursor, can be <code>null</code>.
   */
  public final void setSelectedCursor( Cursor aCursor )
  {
    Cursor old;
    do
    {
      old = this.selectedCursorRef.get();
    }
    while ( !this.selectedCursorRef.compareAndSet( old, aCursor ) );
  }

  /**
   * Sets the name of this session.
   * 
   * @param aName
   *          the name of the session to set, can be <code>null</code>.
   */
  public final void setSessionName( String aName )
  {
    this.session.setName( aName );
  }
}
