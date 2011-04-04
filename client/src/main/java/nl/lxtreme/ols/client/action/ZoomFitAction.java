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

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "zoom fit" action, that zooms the diagram to fit entirely in the
 * current viewport dimensions.
 */
public class ZoomFitAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ZoomFit";

  // CONSTRUCTORS

  /**
   * Creates a new ZoomFitAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public ZoomFitAction( final IClientController aController )
  {
    super( ID, aController, ICON_ZOOM_FIT_BEST, "Zoom to fit", "Zoom to best fit" );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_F ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_F ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getController().zoomToFit();
  }
}

/* EOF */
