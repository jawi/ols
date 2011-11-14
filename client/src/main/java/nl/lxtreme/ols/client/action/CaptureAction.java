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
 * Provides a "capture" action in which the current device controller is asked
 * to start a data capture.
 */
public class CaptureAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "Capture";

  // CONSTRUCTORS

  /**
   * Creates a new CaptureAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public CaptureAction( final ClientController aController )
  {
    this( ID, ICON_CAPTURE_DATA, "Begin capture", "Start capturing data from the logic analyser", aController );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_B ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_B ) );
  }

  /**
   * Creates a new CaptureAction instance.
   * 
   * @param aID
   *          the ID of this action;
   * @param aIconName
   *          the name of the icon to use for this action;
   * @param aName
   *          the name of this action;
   * @param aDescription
   *          the description (tooltip) to use for this action;
   * @param aController
   *          the controller to use for this action.
   */
  protected CaptureAction( final String aID, final String aIconName, final String aName, final String aDescription,
      final ClientController aController )
  {
    super( aID, aController, aIconName, aName, aDescription );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public final void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    if ( !getController().isDeviceSelected() )
    {
      JOptionPane.showMessageDialog( owner, "No capturing device was set!", "Acquisition error",
          JOptionPane.ERROR_MESSAGE );
      return;
    }

    getController().captureData( owner );
  }
}

/* EOF */
