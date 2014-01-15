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
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a "capture" action in which the current device controller is asked
 * to start a data capture.
 */
public class AcquireDataAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "Capture";

  // CONSTRUCTORS

  /**
   * Creates a new {@link AcquireDataAction} instance.
   */
  public AcquireDataAction()
  {
    super( ID );

    putValue( NAME, "Begin capture" );
    putValue( SHORT_DESCRIPTION, "Start capturing data from the logic analyser" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_CAPTURE_DATA ) );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_B ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_B ) );

    putValue( MENU_NAME, ClientConstants.CAPTURE_MENU );
    putValue( MENU_ORDER, 0 );

    putValue( TOOLBAR_GROUP, ClientConstants.ACQUISITION_GROUP );
    putValue( TOOLBAR_ORDER, 2 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public final void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    if ( !client.isDeviceSelected() )
    {
      JOptionPane.showMessageDialog( client, "No capturing device was set!", "Acquisition error",
          JOptionPane.ERROR_MESSAGE );
      return;
    }

    try
    {
      client.acquireData( client );
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
    setEnabled( DeviceState.READY.equals( state ) );
  }

  private void handleException( Client aClient, Exception aException )
  {
    JErrorDialog.showDialog( aClient, "Acquisition failed", aException );
  }
}

/* EOF */
