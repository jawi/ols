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
package nl.lxtreme.ols.client.action;


import java.awt.event.ActionEvent;

import javax.swing.*;

import nl.lxtreme.ols.client.ClientController;


/**
 *
 */
public class SelectDeviceAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "SelectDevice";

  // VARIABLES

  private final String deviceName;

  // CONSTRUCTORS

  /**
   * Creates a new SelectDeviceAction instance.
   *
   * @param aController
   *          the controller to use;
   * @param aDeviceName
   *          the name of the device this action represents.
   */
  public SelectDeviceAction( final ClientController aController, final String aDeviceName )
  {
    super( ID + aDeviceName, aController, aDeviceName, "Selects ".concat( aDeviceName ).concat(
        " as current capturing device" ) );
    this.deviceName = aDeviceName;
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final JMenuItem menuItem = ( JMenuItem )aEvent.getSource();
    if ( menuItem.isSelected() )
    {
      getController().setDeviceController( this.deviceName );
    }
    else
    {
      getController().clearDeviceController();
    }
  }

}

/* EOF */
