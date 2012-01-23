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

import nl.lxtreme.ols.api.data.Cursor;


/**
 * Denotes a cursor with a timestamp, label and index.
 */
public class CursorImpl implements Comparable<CursorImpl>, Cursor
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
   * Creates a new Cursor instance as exact copy of a given cursor.
   * 
   * @param aCursor
   *          the cursor to copy, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public CursorImpl( final Cursor aCursor )
  {
    this( aCursor.getIndex() );

    this.color = aCursor.getColor();
    this.label = aCursor.getLabel();
    this.timestamp = aCursor.isDefined() ? Long.valueOf( aCursor.getTimestamp() ) : null;
  }

  /**
   * Creates a new {@link CursorImpl} instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS}.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public CursorImpl( final int aIndex )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    this.index = aIndex;
    this.color = parseColor( "7bf9dd" ).brighter();
  }

  /**
   * Creates a new {@link CursorImpl} instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS};
   * @param aTimestamp
   *          the timestamp of this cursor.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public CursorImpl( final int aIndex, final long aTimestamp )
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
  public CursorImpl( final int aIndex, final Long aTimestamp )
  {
    this( aIndex );

    this.timestamp = aTimestamp;
  }

  // METHODS

  /**
   * Factory method that creates up to {@value #MAX_CURSORS} cursors. TODO fix
   * me!
   * 
   * @return an array of {@link CursorImpl} instances, never <code>null</code>.
   */
  public static Cursor[] createCursors()
  {
    Cursor[] cursors = new Cursor[MAX_CURSORS];
    cursors[0] = new CursorImpl( 0, 100 );
    cursors[0].setLabel( "AAA" );
    cursors[1] = new CursorImpl( 1, 200 );
    cursors[1].setLabel( "BBB" );
    cursors[2] = new CursorImpl( 2, 125 );
    cursors[2].setLabel( "CCC" );
    cursors[3] = new CursorImpl( 3 );
    cursors[4] = new CursorImpl( 4 );
    cursors[5] = new CursorImpl( 5 );
    cursors[6] = new CursorImpl( 6 );
    cursors[7] = new CursorImpl( 7 );
    cursors[8] = new CursorImpl( 8 );
    cursors[9] = new CursorImpl( 9 );
    return cursors;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clear()
  {
    this.timestamp = null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int compareTo( final CursorImpl aOtherCursor )
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

    final CursorImpl other = ( CursorImpl )aObject;
    if ( this.index != other.index )
    {
      return false;
    }

    return true;
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
  public String getLabel()
  {
    return this.label;
  }

  /**
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
  public boolean hasLabel()
  {
    return ( this.label != null ) && !this.label.trim().isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
  public boolean isDefined()
  {
    return this.timestamp != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setColor( final Color aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Parameter color cannot be null!" );
    }
    this.color = aColor;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setLabel( final String aLabel )
  {
    this.label = aLabel;
  }

  /**
   * {@inheritDoc}
   */
  @Override
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
