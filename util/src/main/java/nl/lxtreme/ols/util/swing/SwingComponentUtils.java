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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.util.swing;

import javax.swing.*;

/**
 * Provides some utility methods for use with Swing components.
 */
public final class SwingComponentUtils
{
  // CONSTANTS

  /**
   * Creates a new {@link SwingComponentUtils} instance, never used.
   */
  private SwingComponentUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Sets the selected item of the given checkbox to the value given, unless
   * this value is <code>null</code>.
   * 
   * @param aCheckBox
   *          the checkbox to set, cannot be <code>null</code>;
   * @param aValue
   *          the value to set, may be <code>null</code>.
   */
  public static void setSelected( final JCheckBox aCheckBox, final Object aValue )
  {
    if ( aCheckBox == null )
    {
      throw new IllegalArgumentException( "CheckBox cannot be null!" );
    }

    if ( aValue != null )
    {
      aCheckBox.setSelected( "true".equalsIgnoreCase( String.valueOf( aValue ) ) );
    }
  }

  /**
   * Sets the selected item of the given combobox to the value given, unless
   * this value is <code>null</code>.
   * 
   * @param aComboBox
   *          the combobox to set, cannot be <code>null</code>;
   * @param aValue
   *          the value to set, may be <code>null</code>.
   */
  public static void setSelectedItem( final JComboBox aComboBox, final Object aValue )
  {
    if ( aComboBox == null )
    {
      throw new IllegalArgumentException( "Combobox cannot be null!" );
    }

    if ( aValue != null )
    {
      aComboBox.setSelectedItem( aValue );
    }
  }

  /**
   * Sets the selected item of the given combobox to the value given, unless
   * this value is <code>null</code> in which case a default value is set.
   * 
   * @param aComboBox
   *          the combobox to set, cannot be <code>null</code>;
   * @param aValue
   *          the value to set, may be <code>null</code>;
   * @param aDefault
   *          the default value to set in case the given value was
   *          <code>null</code>.
   */
  public static void setSelectedItem( final JComboBox aComboBox, final Object aValue, final Object aDefault )
  {
    if ( aComboBox == null )
    {
      throw new IllegalArgumentException( "Combobox cannot be null!" );
    }
    if ( aDefault == null )
    {
      throw new IllegalArgumentException( "Default value cannot be null!" );
    }

    if ( aValue != null )
    {
      aComboBox.setSelectedItem( aValue );
    }
    else
    {
      aComboBox.setSelectedItem( aDefault );
    }
  }
}
