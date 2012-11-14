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
package nl.lxtreme.ols.client.signaldisplay.cursor;


import java.awt.*;
import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.Cursor;


/**
 * Provides a UI-wrapper for {@link Cursor}s.
 */
public final class CursorElement
{
  // VARIABLES

  private final Cursor cursor;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CursorElement} instance.
   */
  public CursorElement( final Cursor aCursor )
  {
    this.cursor = aCursor;
  }

  // METHODS

  /**
   * Clears this cursor, making it undefined.
   */
  public void clear()
  {
    this.cursor.clear();
  }

  /**
   * Returns the color of this cursor.
   * 
   * @return the current color, never <code>null</code>.
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
   * Returns the index of this cursor.
   * 
   * @return an index, >= 0 && < 10.
   */
  public int getIndex()
  {
    return this.cursor.getIndex();
  }

  /**
   * Returns the label of this cursor.
   * 
   * @return the label, can be <code>null</code> if no label is (yet) defined.
   */
  public String getLabel()
  {
    return this.cursor.getLabel();
  }

  /**
   * Returns the time stamp of this cursor.
   * 
   * @return a time stamp.
   * @throws IllegalStateException
   *           in case this cursor is undefined.
   * @see #isDefined()
   */
  public long getTimestamp()
  {
    return this.cursor.getTimestamp();
  }

  /**
   * Returns whether or not this cursor has a label.
   * 
   * @return <code>true</code> if there's a label assigned to this cursor,
   *         <code>false</code> otherwise.
   */
  public boolean hasLabel()
  {
    return this.cursor.hasLabel();
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

    final long timestamp = getTimestamp();
    final double min = timestamp - aDelta;
    final double max = timestamp + aDelta;

    return ( ( aTimestamp >= min ) && ( aTimestamp <= max ) );
  }

  /**
   * Returns whether or not this cursor has a defined time stamp.
   * 
   * @return <code>true</code> if this cursor has a defined time stamp,
   *         <code>false</code> otherwise.
   */
  public boolean isDefined()
  {
    return this.cursor.isDefined();
  }

  /**
   * Sets the color for this cursor.
   * 
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   */
  public void setColor( final Color aColor )
  {
    UIManager.put( getColorKey(), aColor );
  }

  /**
   * Sets the label.
   * 
   * @param aLabel
   *          the label to set
   */
  public void setLabel( final String aLabel )
  {
    this.cursor.setLabel( aLabel );
  }

  /**
   * Sets the time stamp for this cursor.
   * 
   * @param aTimestamp
   *          the time stamp to set, >= 0.
   */
  public void setTimestamp( final long aTimestamp )
  {
    this.cursor.setTimestamp( aTimestamp );
  }

  /**
   * @return a key to lookup the cursor color in the {@link UIManager}.
   */
  private String getColorKey()
  {
    return "ols.cursor." + this.cursor.getIndex() + ".default.color";
  }
}
