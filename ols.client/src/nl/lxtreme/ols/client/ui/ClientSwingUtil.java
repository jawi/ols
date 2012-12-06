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
package nl.lxtreme.ols.client.ui;


import java.awt.*;
import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.Cursor;


/**
 * Provides some common utility methods for the client Swing UI.
 */
public final class ClientSwingUtil
{
  // INNER TYPES

  /**
   * 
   */
  static class CursorLabelRenderer extends DefaultListCellRenderer
  {
    private static final long serialVersionUID = 1L;

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getListCellRendererComponent( final JList aList, Object aValue, final int aIndex, final boolean aIsSelected,
        final boolean aCellHasFocus )
    {
      if ( aValue instanceof Cursor )
      {
        final Cursor cursor = ( Cursor )aValue;
        final Integer index = Integer.valueOf( cursor.getIndex() );

        if ( cursor.hasLabel() )
        {
          aValue = String.format( "%d: %s", index, cursor.getLabel() );
        }
        else
        {
          aValue = String.format( "%d: -", index );
        }
      }
      return super.getListCellRendererComponent( aList, aValue, aIndex, aIsSelected, aCellHasFocus );
    }
  }

  // CONSTRUCTORS

  /**
   * Creates a new {@link ClientSwingUtil} instance.
   */
  private ClientSwingUtil()
  {
    // Not used.
  }

  // METHODS

  /**
   * Creates a dropdown for choosing a valid (defined) cursor.
   * 
   * @param aCursors
   *          the list with cursors, cannot be <code>null</code>;
   * @param aSelectedIndex
   *          the default selected index, >= 0.
   * @return a cursor selector dropdown.
   */
  public static JComboBox createCursorSelector( final Cursor[] aCursors, final int aSelectedIndex )
  {
    return internalCreateCursorSelector( aCursors, aSelectedIndex, false /* aAddUnusedOption */);
  }

  /**
   * Creates a dropdown for choosing an optional (defined) cursor.
   * 
   * @param aCursors
   *          the list with cursors, cannot be <code>null</code>;
   * @param aSelectedIndex
   *          the default selected index, >= 0.
   * @return a cursor selector dropdown.
   */
  public static JComboBox createOptionalCursorSelector( final Cursor[] aCursors, final int aSelectedIndex )
  {
    return internalCreateCursorSelector( aCursors, aSelectedIndex, true /* aAddUnusedOption */);
  }

  /**
   * @param aCursors
   *          the list with cursors, cannot be <code>null</code>;
   * @param aSelectedIndex
   *          the default selected index, >= 0;
   * @param aAddUnusedOption
   *          <code>true</code> if an "Unused" option should be added,
   *          <code>false</code> otherwise.
   */
  private static JComboBox internalCreateCursorSelector( final Cursor[] aCursors, final int aSelectedIndex,
      final boolean aAddUnusedOption )
  {
    Vector<Object> names = new Vector<Object>();
    if ( aAddUnusedOption )
    {
      names.add( "Unused" );
    }
    for ( Cursor cursor : aCursors )
    {
      if ( cursor.isDefined() )
      {
        names.add( cursor );
      }
    }

    final JComboBox result = new JComboBox( names );
    result.setRenderer( new CursorLabelRenderer() );
    if ( aSelectedIndex >= 0 )
    {
      result.setSelectedIndex( aSelectedIndex % names.size() );
    }
    return result;
  }
}
