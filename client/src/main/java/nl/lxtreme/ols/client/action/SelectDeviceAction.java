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
package nl.lxtreme.ols.client.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;


/**
 * Provides an action that selects a particular capturing device.
 */
public class SelectDeviceAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "SelectDevice.";

  // VARIABLES

  private final String deviceName;

  // CONSTRUCTORS

  /**
   * Creates a new SelectDeviceAction instance.
   * 
   * @param aController
   *          the controller to use;
   * @param aDeviceName
   *          the name of the device this action represents; not null.
   */
  public SelectDeviceAction( final ClientController aController, final String aDeviceName )
  {
    super( getID( aDeviceName ), aController, aDeviceName, "Selects ".concat( aDeviceName ).concat(
        " as current capturing device" ) );
    this.deviceName = aDeviceName;
    // if the first character of the name isAlpha, use it as mnemonic
    // (there is no direct conversion between char and KeyEvent available yet)
    char mnemonic = Character.toUpperCase( aDeviceName.charAt( 0 ) );
    if ( Character.isLetterOrDigit( mnemonic ) )
    {
      putValue( MNEMONIC_KEY, Integer.valueOf( mnemonic ) );
    }
  }

  // METHODS

  /**
   * Creates an ID for an action that represents the "select device" action for
   * the device with the given name.
   * 
   * @param aDeviceName
   *          the name of the device to create the ID for, cannot be
   *          <code>null</code>.
   * @return a ID, never <code>null</code>.
   */
  public static final String getID( final String aDeviceName )
  {
    return ID.concat( aDeviceName );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final JMenuItem menuItem = ( JMenuItem )aEvent.getSource();
    getController().selectDevice( menuItem.isSelected() ? this.deviceName : null );
  }

}

/* EOF */
