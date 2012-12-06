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
package nl.lxtreme.ols.client.ui.signaldisplay.marker;


import java.awt.*;
import java.beans.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.signaldisplay.view.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.Cursor;


/**
 * Provides a UI-wrapper for {@link Cursor}s and triggers.
 */
public final class Marker implements Comparable<Marker>
{
  // VARIABLES

  private final Cursor cursor;
  private final long trigger;
  private final PropertyChangeSupport pcs;

  private volatile String label;

  // CONSTRUCTORS

  /**
   * Creates a new {@link Marker} instance representing a cursor.
   * 
   * @param aCursor
   *          the cursor to wrap, cannot be <code>null</code>.
   */
  public Marker( final Cursor aCursor )
  {
    this.cursor = aCursor;
    this.trigger = Ols.NOT_AVAILABLE;
    this.label = null;
    this.pcs = new PropertyChangeSupport( this );
  }

  /**
   * Creates a new {@link Marker} instance representing a trigger.
   * 
   * @param aTrigger
   *          the trigger to wrap, can be <code>null</code> if no trigger is
   *          present in the original data.
   */
  public Marker( final long aTrigger )
  {
    this.cursor = null;
    this.trigger = ( aTrigger < 0L ) ? Ols.NOT_AVAILABLE : aTrigger;
    this.label = "Trigger";
    this.pcs = new PropertyChangeSupport( this );
  }

  // METHODS

  /**
   * Adds a given listener to the list of property change listeners.
   * 
   * @param aListener
   *          the listener to add, can be <code>null</code> in which case this
   *          method does nothing.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.pcs.addPropertyChangeListener( aListener );
  }

  /**
   * Clears this marker, making it undefined.
   * 
   * @throws IllegalStateException
   *           if this marker cannot be moved.
   * @see #isMoveable()
   */
  public void clear()
  {
    setTimestamp( Ols.NOT_AVAILABLE );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final Marker aMarker )
  {
    int result = getIndex() - aMarker.getIndex();
    return result;
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
    if ( ( aObject == null ) || !( aObject instanceof Marker ) )
    {
      return false;
    }

    Marker other = ( Marker )aObject;
    if ( this.cursor == null )
    {
      if ( other.cursor != null )
      {
        return false;
      }
    }
    else if ( !this.cursor.equals( other.cursor ) )
    {
      return false;
    }
    if ( this.trigger != other.trigger )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the color of this marker.
   * 
   * @return a color, never <code>null</code>.
   */
  public Color getColor()
  {
    Color result = UIManager.getColor( getColorKey() );
    if ( result == null )
    {
      result = Color.WHITE;
    }
    return result;
  }

  /**
   * Returns the index of this marker.
   * 
   * @return an index, >= 0 && < 10.
   */
  public int getIndex()
  {
    if ( isCursor() )
    {
      return this.cursor.getIndex();
    }
    // There's only one trigger moment for now...
    return 0;
  }

  /**
   * Returns the label of this marker.
   * 
   * @return a label, can be <code>null</code> if no label is (yet) defined.
   */
  public String getLabel()
  {
    if ( isCursor() )
    {
      return this.cursor.getLabel();
    }
    return this.label;
  }

  /**
   * Returns the time stamp of this marker.
   * 
   * @return a time stamp.
   * @throws IllegalStateException
   *           in case this marker is undefined.
   * @see #isDefined()
   */
  public long getTimestamp()
  {
    if ( isCursor() )
    {
      return this.cursor.getTimestamp();
    }
    if ( this.trigger < 0L )
    {
      throw new IllegalStateException( "Trigger is not defined!" );
    }
    return this.trigger;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( ( this.cursor == null ) ? 0 : this.cursor.hashCode() );
    result = ( prime * result ) + ( int )( this.trigger ^ ( this.trigger >>> 32 ) );
    return result;
  }

  /**
   * Returns whether or not this marker has a label.
   * 
   * @return <code>true</code> if there's a label assigned to this marker,
   *         <code>false</code> otherwise.
   */
  public boolean hasLabel()
  {
    String l = getLabel();
    return ( l != null ) && !"".equals( l.trim() );
  }

  /**
   * Returns whether the time stamp of this marker falls within the range of a
   * given time stamp.
   * 
   * @param aTimestamp
   *          the time stamp to check against;
   * @param aDelta
   *          the delta deviation between this time stamp and the given time
   *          stamp, >= 0.
   * @return <code>true</code> if this marker is within the area of the given
   *         time stamp, <code>false</code> otherwise.
   */
  public boolean inArea( final long aTimestamp, final double aDelta )
  {
    if ( !isDefined() )
    {
      return false;
    }

    final long timestamp = getTimestamp();
    final double min = timestamp - aDelta;
    final double max = timestamp + aDelta;

    return ( ( aTimestamp >= min ) && ( aTimestamp <= max ) );
  }

  /**
   * Returns whether or not this marker has a defined time stamp.
   * 
   * @return <code>true</code> if this marker has a defined time stamp,
   *         <code>false</code> otherwise.
   */
  public boolean isDefined()
  {
    if ( isCursor() )
    {
      return this.cursor.isDefined();
    }
    return this.trigger >= 0L;
  }

  /**
   * Returns whether or not this marker can be moved in time.
   * 
   * @return <code>true</code> if this marker can be moved, <code>false</code>
   *         if it is static.
   */
  public boolean isMoveable()
  {
    return isCursor();
  }

  /**
   * Removes a given listener from the list of property change listeners.
   * 
   * @param aListener
   *          the listener to remove, can be <code>null</code> in which case
   *          this method does nothing.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.pcs.removePropertyChangeListener( aListener );
  }

  /**
   * Sets the color for this marker.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( final Color aColor )
  {
    final Color oldColor = getColor();
    UIManager.put( getColorKey(), aColor );
    this.pcs.fireIndexedPropertyChange( "color", getIndex(), oldColor, aColor );
  }

  /**
   * Sets the label for this marker.
   * 
   * @param aLabel
   *          the label to set, can be <code>null</code> to remove the label.
   */
  public void setLabel( final String aLabel )
  {
    String oldLabel = getLabel();
    if ( isCursor() )
    {
      this.cursor.setLabel( aLabel );
    }
    else
    {
      this.label = aLabel;
    }
    this.pcs.fireIndexedPropertyChange( "label", getIndex(), oldLabel, aLabel );
  }

  /**
   * Sets the time stamp for this marker.
   * 
   * @param aTimestamp
   *          the time stamp to set, >= 0.
   * @throws IllegalStateException
   *           in case this marker cannot be moved.
   * @see #isMoveable()
   */
  public void setTimestamp( final long aTimestamp )
  {
    // Only supported for cursors...
    Long oldTimestamp = isDefined() ? Long.valueOf( getTimestamp() ) : null;
    if ( isCursor() )
    {
      this.cursor.setTimestamp( aTimestamp );
    }
    else
    {
      throw new IllegalStateException( "Marker cannot be moved!" );
    }
    this.pcs.fireIndexedPropertyChange( "timestamp", getIndex(), oldTimestamp, Long.valueOf( aTimestamp ) );
  }

  /**
   * @return a key to lookup the marker color in the {@link UIManager}.
   */
  private String getColorKey()
  {
    if ( isCursor() )
    {
      return "ols.cursor." + getIndex() + ".default.color";
    }
    return UIManagerKeys.SIGNALVIEW_TRIGGER_COLOR;
  }

  /**
   * Returns whether or not this marker represents a cursor.
   * 
   * @return <code>true</code> if this marker actually represents a cursor,
   *         <code>false</code> otherwise.
   */
  private boolean isCursor()
  {
    return this.cursor != null;
  }
}
