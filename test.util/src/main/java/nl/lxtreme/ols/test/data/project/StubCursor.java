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
package nl.lxtreme.ols.test.data.project;


import java.awt.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.Cursor;


/**
 * Denotes a cursor with a timestamp, label and index.
 */
public final class StubCursor implements Comparable<StubCursor>, Cloneable, Cursor
{
  // VARIABLES

  private final int index;
  private String label;
  private Long timestamp;
  private Color color;

  // CONSTRUCTORS

  /**
   * Creates a new {@link StubCursor} instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS}.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public StubCursor( final int aIndex )
  {
    if ( ( aIndex < 0 ) || ( aIndex >= Ols.MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    this.index = aIndex;
    this.color = Color.WHITE;
  }

  /**
   * Creates a new {@link StubCursor} instance.
   * 
   * @param aIndex
   *          the index of this cursor, >= 0 && < {@value #MAX_CURSORS};
   * @param aTimestamp
   *          the timestamp of this cursor.
   * @throws IllegalArgumentException
   *           in case the given index was invalid.
   */
  public StubCursor( final int aIndex, final long aTimestamp )
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
  public StubCursor( final int aIndex, final Long aTimestamp )
  {
    this( aIndex );

    this.timestamp = aTimestamp;
  }

  // METHODS

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
  public StubCursor clone()
  {
    try
    {
      final StubCursor result = ( StubCursor )super.clone();
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
  public int compareTo( final StubCursor aOtherCursor )
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

    final StubCursor other = ( StubCursor )aObject;
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
