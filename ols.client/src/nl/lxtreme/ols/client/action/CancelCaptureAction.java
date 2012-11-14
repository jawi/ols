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

import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.icons.*;


/**
 * Provides an action to cancel an ongoing acquisition.
 */
public class CancelCaptureAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "CancelCapture";

  // CONSTRUCTORS

  /**
   * Creates a new {@link CancelCaptureAction} instance.
   */
  public CancelCaptureAction()
  {
    putValue( NAME, "Cancel capture" );
    putValue( SHORT_DESCRIPTION, "Cancel the current capture" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_CANCEL_CAPTURE ) );
    putValue( ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_ESCAPE, 0 ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_C ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getAcquisitionController().cancelCapture();
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
    setEnabled( getAcquisitionController().isDeviceCapturing() );
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
