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
package nl.lxtreme.ols.client2.views.state;


import java.util.*;

import javax.swing.event.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides the table model for displaying the states.
 */
class StateTableModel extends AbstractTableModel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final int[] values;
  private final long[] timestamps;
  private final ChannelGroup[] groups;
  private final Radix[] viewModes;

  // CONSTRUCTORS

  /**
   * Creates a new {@link StateTableModel} instance.
   */
  public StateTableModel( AcquisitionData aData )
  {
    this.values = aData.getValues();
    this.timestamps = aData.getTimestamps();
    this.groups = aData.getChannelGroups();

    this.viewModes = new Radix[this.groups.length];
    Arrays.fill( this.viewModes, Radix.HEX );
  }

  // METHODS

  /**
   * Convenience method to fire an event when the data of an entire column is
   * changed.
   * 
   * @param aColumnIndex
   *          the index of the column that is changed.
   */
  public void fireColumnDataChanged( int aColumnIndex )
  {
    int first = 0;
    int last = getRowCount() - 1;

    fireTableChanged( new TableModelEvent( this, first, last, aColumnIndex ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<?> getColumnClass( int aColumnIndex )
  {
    if ( aColumnIndex == 0 )
    {
      return Long.class;
    }
    return Integer.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getColumnCount()
  {
    return 1 + ( this.groups != null ? this.groups.length : 0 );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getColumnName( int aColumn )
  {
    String name = super.getColumnName( aColumn );
    String sub = null;
    if ( aColumn == 0 )
    {
      name = "State";
      sub = "#";
    }
    else
    {
      int groupIdx = aColumn - 1;
      if ( this.groups != null && groupIdx < this.groups.length )
      {
        ChannelGroup group = this.groups[groupIdx];
        Radix vm = this.viewModes[groupIdx];

        name = group.getName();
        sub = Integer.toString( vm.getBase() );
      }
    }

    StringBuilder sb = new StringBuilder();
    sb.append( "<html><body><b>" ).append( name ).append( "</b>" );
    if ( sub != null )
    {
      sb.append( "<sub>" ).append( sub ).append( "</sub>" );
    }
    sb.append( "</body></html>" );

    return sb.toString();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getRowCount()
  {
    return this.values.length;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getValueAt( int aRowIndex, int aColumnIndex )
  {
    if ( aColumnIndex == 0 )
    {
      return this.timestamps[aRowIndex];
    }

    int groupIdx = aColumnIndex - 1;
    if ( this.groups == null || groupIdx >= this.groups.length )
    {
      return null;
    }

    return this.groups[groupIdx].getValue( this.values[aRowIndex] );
  }

  /**
   * @param aColumnIndex
   * @return
   */
  public Radix getRadix( int aColumnIndex )
  {
    if ( aColumnIndex == 0 )
    {
      return Radix.DEC;
    }
    else
    {
      return this.viewModes[aColumnIndex - 1];
    }
  }

  /**
   * @param aColumnIndex
   * @return
   */
  public int getViewWidth( int aColumnIndex )
  {
    if ( aColumnIndex == 0 )
    {
      return -1;
    }

    int groupIdx = aColumnIndex - 1;

    int channels = this.groups[groupIdx].getChannelCount();
    double width = this.viewModes[groupIdx].getWidth();

    return ( int )Math.ceil( channels / width );
  }

  /**
   * @param aColumnIndex
   * @param aMode
   */
  public void setViewMode( int aColumnIndex, Radix aMode )
  {
    if ( aColumnIndex > 0 )
    {
      this.viewModes[aColumnIndex - 1] = aMode;
    }
  }
}
