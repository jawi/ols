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
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.acquisition.AcquisitionService.*;
import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * @author jawi
 */
public class CancelAcquisitionAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "CancelCapture";

  // CONSTRUCTORS

  /**
   * Creates a new {@link CancelAcquisitionAction} instance.
   */
  public CancelAcquisitionAction()
  {
    super( ID );

    putValue( NAME, "Cancel capture" );
    putValue( SHORT_DESCRIPTION, "Cancel the current capture" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_CANCEL_CAPTURE ) );

    putValue( ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE, 0 ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_C ) );

    putValue( MENU_NAME, ClientConstants.CAPTURE_MENU );
    putValue( MENU_ORDER, 1 );
    
    putValue( TOOLBAR_GROUP, ClientConstants.ACQUISITION_GROUP );
    putValue( TOOLBAR_ORDER, 3 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    try
    {
      client.cancelAcquisition();
    }
    catch ( IllegalStateException exception )
    {
      handleException( client, exception );
    }
    catch ( IOException exception )
    {
      handleException( client, exception );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    DeviceState state = aClient.getDeviceState();
    setEnabled( DeviceState.ACQUIRING.equals( state ) );
  }

  private void handleException( Client aClient, Exception aException )
  {
    JErrorDialog.showDialog( aClient, "Cancel failed", aException );
  }
}
