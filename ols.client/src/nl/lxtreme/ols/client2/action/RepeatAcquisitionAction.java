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

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a "repeat capture" action which simply repeats the capture with the
 * current settings.
 */
public class RepeatAcquisitionAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "RepeatCapture";

  // CONSTRUCTORS

  /**
   * Creates a new {@link RepeatAcquisitionAction} instance.
   */
  public RepeatAcquisitionAction()
  {
    super( ID );

    putValue( NAME, "Repeat capture" );
    putValue( SHORT_DESCRIPTION, "Repeat capture with current device settings" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_RECAPTURE_DATA ) );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_R ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_R ) );

    putValue( MENU_NAME, ClientConstants.CAPTURE_MENU );
    putValue( MENU_ORDER, 1 );
    
    putValue( TOOLBAR_GROUP, ClientConstants.ACQUISITION_GROUP );
    putValue( TOOLBAR_ORDER, 4 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    if ( !client.isDeviceSelected() )
    {
      JOptionPane.showMessageDialog( client, "No capturing device found!", "Capture error", JOptionPane.ERROR_MESSAGE );
      return;
    }
    if ( !client.isDeviceSetup() )
    {
      JOptionPane.showMessageDialog( client, "Capturing device is not setup!", "Capture error", JOptionPane.ERROR_MESSAGE );
      return;
    }

    try
    {
      client.repeatAcquireData();
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
    setEnabled( aClient.isDeviceSelected() && aClient.isDeviceSetup() );
  }

  private void handleException( Client aClient, Exception aException )
  {
    JErrorDialog.showDialog( aClient, "Acquisition failed", aException );
  }
}

/* EOF */
