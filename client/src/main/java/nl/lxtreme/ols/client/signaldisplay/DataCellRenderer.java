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
package nl.lxtreme.ols.client.signaldisplay;


import java.awt.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client.*;


/**
 * Provides a cell renderer for data values.
 */
class DataCellRenderer extends DefaultTableCellRenderer
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // METHODS

  @Override
  public Component getTableCellRendererComponent( final JTable aTable, final Object aValue, final boolean aIsSelected,
      final boolean aHasFocus, final int aRow, final int aColumn )
  {
    JLabel label = ( JLabel )super
        .getTableCellRendererComponent( aTable, aValue, aIsSelected, aHasFocus, aRow, aColumn );

    StateTableModel tableModel = ( StateTableModel )aTable.getModel();

    Radix radix = tableModel.getRadix( aColumn );
    int width = tableModel.getViewWidth( aColumn );

    label.setHorizontalAlignment( SwingConstants.RIGHT );
    label.setText( radix.toString( ( ( Integer )aValue ).intValue(), width ) );
    return label;
  }
}
