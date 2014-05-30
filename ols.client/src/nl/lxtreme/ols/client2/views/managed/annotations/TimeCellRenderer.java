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
package nl.lxtreme.ols.client2.views.managed.annotations;


import java.awt.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.common.Unit.Value;


/**
 * Provides a table cell renderer for relative time.
 */
class TimeCellRenderer extends DefaultTableCellRenderer
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // METHODS

  @Override
  public Component getTableCellRendererComponent( JTable aTable, Object aValue, boolean aIsSelected, boolean aHasFocus,
      int aRow, int aColumn )
  {
    JLabel label = ( JLabel )super.getTableCellRendererComponent( aTable, aValue, aIsSelected, //
        aHasFocus, aRow, aColumn );

    Object value = aValue;
    if ( value instanceof Double )
    {
      Value time = Value.asTime( ( Integer )value );
      label.setText( String.format( "%#.2s", time ) );
    }

    label.setHorizontalAlignment( SwingConstants.RIGHT );

    return label;
  }
}
