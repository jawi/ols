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


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "repeat capture" action which simply repeats the capture with the
 * current settings.
 */
public class RepeatCaptureAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "RepeatCapture";

  // CONSTRUCTORS

  /**
   * Creates a new RepeatCaptureAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public RepeatCaptureAction( final ClientController aController )
  {
    super( ID, aController, ICON_RECAPTURE_DATA, "Repeat capture", "Repeat capture with current device settings" );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_R ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_R ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    if ( !getController().isDeviceSelected() )
    {
      JOptionPane.showMessageDialog( owner, "No capturing device found!", "Capture error", JOptionPane.ERROR_MESSAGE );
      return;
    }
    if ( !getController().isDeviceSetup() )
    {
      JOptionPane.showMessageDialog( owner, "Capturing device is not setup!", "Capture error",
          JOptionPane.ERROR_MESSAGE );
      return;
    }

    getController().repeatCaptureData();
  }
}

/* EOF */
