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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.cursors;


import java.util.*;

import javax.swing.table.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor.LabelStyle;


/**
 * Provides a table model to show cursor information.
 */
class CursorTableModel extends AbstractTableModel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Cursor[] cursors;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CursorTableModel} instance.
   */
  public CursorTableModel()
  {
    this( new Cursor[0] );
  }

  /**
   * Creates a new {@link CursorTableModel} instance.
   * 
   * @param aCursors
   *          the cursors for this table model.
   */
  public CursorTableModel( Cursor[] aCursors )
  {
    List<Cursor> _cursors = new ArrayList<Cursor>( aCursors.length );
    for ( Cursor cursor : aCursors )
    {
      if ( cursor.isDefined() )
      {
        _cursors.add( cursor );
      }
    }
    this.cursors = _cursors.toArray( new Cursor[_cursors.size()] );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( Object aObject )
  {
    if ( aObject == this )
    {
      return true;
    }
    if ( aObject == null || !( aObject instanceof CursorTableModel ) )
    {
      return false;
    }

    CursorTableModel that = ( CursorTableModel )aObject;
    return Arrays.deepEquals( this.cursors, that.cursors );
  }

  /**
   * Returns the cursor at a given row.
   * 
   * @param aRowIndex
   *          the index of the row to return the cursor for.
   * @return the requested cursor, never <code>null</code>.
   */
  public Cursor getCursor( int aRowIndex )
  {
    return this.cursors[aRowIndex];
  }

  /**
   * @param aCursor
   * @return
   */
  public int getRowIndex( Cursor aCursor )
  {
    for ( int i = 0; i < this.cursors.length; i++ )
    {
      if ( this.cursors[i].equals( aCursor ) )
      {
        return i;
      }
    }
    return -1;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<?> getColumnClass( int aColumnIndex )
  {
    if ( aColumnIndex == 0 )
    {
      return Integer.class;
    }
    return String.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getColumnCount()
  {
    return 3;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getColumnName( int aColumnIndex )
  {
    if ( aColumnIndex == 0 )
    {
      return "#";
    }
    else if ( aColumnIndex == 1 )
    {
      return "Time";
    }
    else if ( aColumnIndex == 2 )
    {
      return "Label";
    }
    return super.getColumnName( aColumnIndex );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getRowCount()
  {
    return this.cursors.length;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getValueAt( int aRowIndex, int aColumnIndex )
  {
    Cursor cursor = this.cursors[aRowIndex];
    if ( aColumnIndex == 0 )
    {
      return cursor.getIndex();
    }
    else if ( aColumnIndex == 1 )
    {
      return cursor.getLabel( LabelStyle.TIME_ONLY );
    }
    return cursor.getLabel();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    return Arrays.deepHashCode( this.cursors );
  }
}
