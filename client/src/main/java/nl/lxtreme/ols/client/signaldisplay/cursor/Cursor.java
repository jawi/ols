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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.cursor;


import static nl.lxtreme.ols.util.ColorUtils.*;

import java.awt.*;


/**
 * Denotes a cursor with a timestamp, label and index.
 */
public class Cursor implements Comparable<Cursor>, Cloneable
{
  // CONSTANTS

  public static final int MAX_CURSORS = 10;

  // VARIABLES

  private final int index;
  private String label;
  private Long timestamp;
  private Color color;

  // CONSTRUCTORS

  /**
   * Creates a new Cursor instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS}.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public Cursor( final int aIndex )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    this.index = aIndex;
    this.color = parseColor( "7bf9dd" ).brighter();
  }

  /**
   * Creates a new Cursor instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS};
   * @param aTimestamp
   *          the timestamp of this cursor.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public Cursor( final int aIndex, final long aTimestamp )
  {
    this( aIndex );

    this.timestamp = Long.valueOf( aTimestamp );
  }

  /**
   * Creates a new Cursor instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS};
   * @param aTimestamp
   *          the timestamp of this cursor, can be <code>null</code> in case
   *          this cursor is undefined.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public Cursor( final int aIndex, final Long aTimestamp )
  {
    this( aIndex );

    this.timestamp = aTimestamp;
  }

  // METHODS

  /**
   * Factory method that creates up to {@value #MAX_CURSORS} cursors.
   * 
   * @return an array of {@link Cursor} instances, never <code>null</code>.
   */
  public static Cursor[] createCursors()
  {
    Cursor[] cursors = new Cursor[MAX_CURSORS];
    cursors[0] = new Cursor( 0, 100 );
    cursors[0].setLabel( "AAA" );
    cursors[1] = new Cursor( 1, 200 );
    cursors[1].setLabel( "BBB" );
    cursors[2] = new Cursor( 2, 125 );
    cursors[2].setLabel( "CCC" );
    cursors[3] = new Cursor( 3 );
    cursors[4] = new Cursor( 4 );
    cursors[5] = new Cursor( 5 );
    cursors[6] = new Cursor( 6 );
    cursors[7] = new Cursor( 7 );
    cursors[8] = new Cursor( 8 );
    cursors[9] = new Cursor( 9 );
    return cursors;
  }

  /**
   * Clears this cursor, making it undefined.
   */
  public void clear()
  {
    this.timestamp = null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Cursor clone()
  {
    try
    {
      final Cursor result = ( Cursor )super.clone();
      if ( isDefined() )
      {
        result.setTimestamp( this.timestamp.longValue() );
      }
      if ( hasLabel() )
      {
        result.setLabel( new String( this.label ) );
      }
      result.setColor( this.color );
      return result;
    }
    catch ( CloneNotSupportedException exception )
    {
      throw new RuntimeException( "Internal error: Object#clone() not supported?!" );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final Cursor aOtherCursor )
  {
    return this.index - aOtherCursor.index;
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
    if ( ( aObject == null ) || ( getClass() != aObject.getClass() ) )
    {
      return false;
    }

    final Cursor other = ( Cursor )aObject;
    if ( this.index != other.index )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the cursor color.
   * 
   * @return the color of this cursor, never <code>null</code>.
   */
  public Color getColor()
  {
    return this.color;
  }

  /**
   * Returns the current value of index.
   * 
   * @return the index
   */
  public int getIndex()
  {
    return this.index;
  }

  /**
   * Returns the current value of label.
   * 
   * @return the label
   */
  public String getLabel()
  {
    return this.label;
  }

  /**
   * Returns the current value of timestamp.
   * 
   * @return the timestamp, can be <code>null</code> if this cursor is
   *         undefined.
   * @throws IllegalStateException
   *           in case this cursor is undefined.
   * @see #isDefined()
   */
  public long getTimestamp()
  {
    if ( this.timestamp == null )
    {
      throw new IllegalStateException( "Cannot get timestamp on undefined cursor!" );
    }
    return this.timestamp.longValue();
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
   * Returns whether or not this cursor has a label.
   * 
   * @return <code>true</code> if there's a label assigned to this cursor,
   *         <code>false</code> otherwise.
   */
  public boolean hasLabel()
  {
    return ( this.label != null ) && !this.label.trim().isEmpty();
  }

  /**
   * Returns whether the time stamp of this cursor falls within the range of a
   * given time stamp.
   * 
   * @param aTimestamp
   *          the time stamp to check against;
   * @param aDelta
   *          the delta deviation between this time stamp and the given time
   *          stamp, >= 0.
   * @return <code>true</code> if this cursor is within the area of the given
   *         time stamp, <code>false</code> otherwise.
   */
  public boolean inArea( final long aTimestamp, final double aDelta )
  {
    if ( !isDefined() )
    {
      return false;
    }

    final double min = this.timestamp.longValue() - aDelta;
    final double max = this.timestamp.longValue() + aDelta;

    return ( ( aTimestamp >= min ) && ( aTimestamp <= max ) );
  }

  /**
   * Returns whether or not this cursor has a defined timestamp.
   * 
   * @return <code>true</code> if this cursor has a defined timestamp,
   *         <code>false</code> otherwise.
   */
  public boolean isDefined()
  {
    return this.timestamp != null;
  }

  /**
   * Sets the color of this cursor.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( final Color aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Parameter color cannot be null!" );
    }
    this.color = aColor;
  }

  /**
   * Sets the label.
   * 
   * @param aLabel
   *          the label to set
   */
  public void setLabel( final String aLabel )
  {
    this.label = aLabel;
  }

  /**
   * Sets the time stamp for this cursor.
   * 
   * @param aTimestamp
   *          the time stamp to set, >= 0.
   */
  public void setTimestamp( final long aTimestamp )
  {
    this.timestamp = Long.valueOf( aTimestamp );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    return this.index + ": " + this.label;
  }
}
