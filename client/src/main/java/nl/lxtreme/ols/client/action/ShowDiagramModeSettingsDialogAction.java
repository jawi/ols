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

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "show diagram mode settings" action.
 */
public class ShowDiagramModeSettingsDialogAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ShowDiagramModeSettingsDialogAction";

  // CONSTRUCTORS

  /**
   * Creates a new ShowDiagramModeSettingsDialogAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public ShowDiagramModeSettingsDialogAction( final IClientController aController )
  {
    super( ID, aController, "Diagram mode settings", "Show the diagram mode settings dialog" );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_M ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );
    getController().showDiagramModeSettingsDialog( owner );
  }
}

/* EOF */
