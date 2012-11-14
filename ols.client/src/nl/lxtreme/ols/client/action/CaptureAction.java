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


import static nl.lxtreme.ols.client.icons.IconLocator.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "capture" action in which the current device controller is asked
 * to start a data capture.
 */
public class CaptureAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "Capture";

  // CONSTRUCTORS

  /**
   * Creates a new {@link CaptureAction} instance.
   */
  public CaptureAction()
  {
    putValue( NAME, "Begin capture" );
    putValue( SHORT_DESCRIPTION, "Start capturing data from the logic analyser" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_CAPTURE_DATA ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_B ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_B ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public final void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );
    getAcquisitionController().captureData( owner );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return ID;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState()
  {
    setEnabled( getAcquisitionController().isDeviceSelected() );
  }

  /**
   * @return the {@link AcquisitionController} instance, never <code>null</code>
   *         .
   */
  private AcquisitionController getAcquisitionController()
  {
    return Client.getInstance().getAcquisitionController();
  }
}

/* EOF */
