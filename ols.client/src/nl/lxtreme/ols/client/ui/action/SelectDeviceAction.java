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
package nl.lxtreme.ols.client.ui.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.*;


/**
 * Provides an action that selects a particular capturing device.
 */
public class SelectDeviceAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "SelectDevice.";

  // VARIABLES

  private final DeviceController deviceController;
  private final String deviceName;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SelectDeviceAction} instance.
   * 
   * @param aController
   *          the controller to use;
   * @param aDeviceName
   *          the name of the device this action represents; not null.
   */
  public SelectDeviceAction( final DeviceController aController, final String aDeviceName )
  {
    this.deviceController = aController;
    this.deviceName = aDeviceName;

    putValue( NAME, aDeviceName );
    putValue( SHORT_DESCRIPTION, "Selects ".concat( aDeviceName ).concat( " as current capturing device" ) );

    // if the first character of the name is a letter or a digit, use it as
    // mnemonic (there is no direct conversion between char and KeyEvent
    // available yet)
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
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    JMenuItem menuItem = ( JMenuItem )aEvent.getSource();

    String newDeviceName = null;
    if ( menuItem.isSelected() )
    {
      newDeviceName = this.deviceName;
    }

    this.deviceController.setSelectedDeviceName( newDeviceName );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return getID( this.deviceName );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState()
  {
    // Always enabled...
    setEnabled( true );
  }
}

/* EOF */
