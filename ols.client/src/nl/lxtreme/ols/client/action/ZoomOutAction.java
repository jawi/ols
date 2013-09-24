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


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "zoom out" action, that zooms out the diagram with a constant
 * factor.
 */
public class ZoomOutAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ZoomOut";

  // VARIABLES

  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new ZoomOutAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public ZoomOutAction( final SignalDiagramController aController )
  {
    this.controller = aController;

    putValue( NAME, "Zoom out" );
    putValue( SHORT_DESCRIPTION, "Zooms out with a factor" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_ZOOM_OUT ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_MINUS ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_O ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getZoomController().zoomOut();
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
   * @return the signal diagram's zoom controller, never <code>null</code>.
   */
  private ZoomController getZoomController()
  {
    return this.controller.getZoomController();
  }
}

/* EOF */
