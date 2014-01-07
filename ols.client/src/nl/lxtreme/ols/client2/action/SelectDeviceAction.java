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
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.device.api.*;


/**
 * Provides an action that selects a particular capturing device.
 */
public class SelectDeviceAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "SelectDevice.";

  // VARIABLES

  private final Device device;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SelectDeviceAction} instance.
   * 
   * @param aDevice
   *          the device this action represents; not null.
   */
  public SelectDeviceAction( Device aDevice )
  {
    super( getID( aDevice ) );

    this.device = aDevice;

    String deviceName = aDevice.getName();

    putValue( NAME, deviceName );
    putValue( SHORT_DESCRIPTION, "Selects ".concat( deviceName ).concat( " as current capturing device" ) );
    putValue( MENU_NAME, ClientConstants.DEVICE_MENU );
    putValue( MENU_GROUPED, Boolean.TRUE );

    // if the first character of the name isAlpha, use it as mnemonic
    // (there is no direct conversion between char and KeyEvent available yet)
    char mnemonic = Character.toUpperCase( deviceName.charAt( 0 ) );
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
  public static final String getID( Device aDevice )
  {
    return ID.concat( aDevice.getName() );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    try
    {
      JMenuItem menuItem = ( JMenuItem )aEvent.getSource();

      client.selectDevice( menuItem.isSelected() ? this.device.getName() : null );
    }
    finally
    {
      client.updateActions();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    // Always enabled.
    boolean selected = this.device.getName().equals( aClient.getSelectedDeviceName() );
    putValue( SELECTED_KEY, Boolean.valueOf( selected ) );
  }
}

/* EOF */
