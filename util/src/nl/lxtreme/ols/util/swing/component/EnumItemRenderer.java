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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;


/**
 * Provides a generic list cell-renderer for enum-constants.
 */
public class EnumItemRenderer<ENUM_TYPE extends Enum<?>> extends DefaultListCellRenderer
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // METHODS

  /**
   * @see javax.swing.DefaultListCellRenderer#getListCellRendererComponent(javax.swing.JList,
   *      java.lang.Object, int, boolean, boolean)
   */
  @Override
  @SuppressWarnings( "unchecked" )
  public Component getListCellRendererComponent( final JList aList, final Object aValue, final int aIndex,
      final boolean aIsSelected, final boolean aCellHasFocus )
  {
    final String text;
    if ( aValue instanceof Enum<?> )
    {
      text = getDisplayValue( ( ENUM_TYPE )aValue );
    }
    else
    {
      text = getFallbackValue( aValue );
    }

    final Component renderer = super.getListCellRendererComponent( aList, text, aIndex, aIsSelected, aCellHasFocus );
    if ( renderer instanceof JComponent )
    {
      final String toolTipText = getToolTip( aValue );
      if ( !StringUtils.isEmpty( toolTipText ) )
      {
        ( ( JComponent )renderer ).setToolTipText( toolTipText );
      }
    }
    return renderer;
  }

  /**
   * Returns the display value for the given enum value.
   * 
   * @param aValue
   *          the enum value to render as display value, cannot be
   *          <code>null</code>.
   * @return a display value string, never <code>null</code>.
   */
  protected String getDisplayValue( final ENUM_TYPE aValue )
  {
    return String.valueOf( aValue );
  }

  /**
   * Returns the display value for the given value, which is <em>no</em> enum
   * value.
   * 
   * @param aValue
   *          the non-enum value to render as string, can be <code>null</code>.
   * @return a display value string, never <code>null</code>.
   */
  protected String getFallbackValue( final Object aValue )
  {
    return ( aValue == null ) ? "" : String.valueOf( aValue );
  }

  /**
   * Returns the tool tip text for the given (enum) value.
   * 
   * @param aValue
   *          the value to get the tool tip for, cannot be <code>null</code>.
   * @return a tool tip text, may be empty or <code>null</code>.
   */
  protected String getToolTip( final Object aValue )
  {
    return null;
  }
}
