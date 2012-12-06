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


import static nl.lxtreme.ols.client.ui.icons.IconLocator.*;

import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.icons.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "repeat capture" action which simply repeats the capture with the
 * current settings.
 */
public class RepeatCaptureAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "RepeatCapture";

  // CONSTRUCTORS

  /**
   * Creates a new {@link RepeatCaptureAction} instance.
   */
  public RepeatCaptureAction()
  {
    putValue( NAME, "Repeat capture" );
    putValue( SHORT_DESCRIPTION, "Repeat capture with current device settings" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_RECAPTURE_DATA ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_R ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_R ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getAcquisitionController().repeatCaptureData();
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
    final AcquisitionController acquisitionController = getAcquisitionController();
    setEnabled( acquisitionController.isDeviceSelected() && acquisitionController.isDeviceSetup() );
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
