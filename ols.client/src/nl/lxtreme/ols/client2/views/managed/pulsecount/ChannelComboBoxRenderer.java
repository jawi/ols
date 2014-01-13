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
package nl.lxtreme.ols.client2.views.managed.pulsecount;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a renderer for the channels combobox.
 */
final class ChannelComboBoxRenderer extends DefaultListCellRenderer
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // METHODS

  @Override
  public Component getListCellRendererComponent( JList aList, Object aValue, int aIndex, boolean aIsSelected, boolean aCellHasFocus )
  {
    StringBuilder sb = new StringBuilder();
    if ( aValue instanceof Channel )
    {
      Channel channel = ( Channel )aValue;

      sb.append( channel.getIndex() );
      if ( channel.hasName() )
      {
        sb.append( ", " ).append( channel.getLabel() );
      }
    }

    return super.getListCellRendererComponent( aList, sb.toString(), aIndex, aIsSelected, aCellHasFocus );
  }
}
