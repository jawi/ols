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
 * Provides a "new project" action.
 */
public class NewProjectAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "NewProject";

  // CONSTRUCTORS

  /**
   * Creates a new NewProjectAction instance.
   * 
   * @param aController
   *          the client controller to use.
   */
  public NewProjectAction( final ClientController aController )
  {
    super( ID, aController, "New project ...", "Create a new project" );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_N ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_N ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window parent = SwingComponentUtils.getOwningWindow( aEvent );

    final ClientController controller = getController();

    // Issue #62: in case the user does NOT confirm to lose its changes, we
    // should bail out immediately, otherwise continue normally...
    if ( controller.isProjectChanged() && //
        !SwingComponentUtils.askConfirmation( parent,
            "Current project has been changed.\nDo you really want to lose your changes?" ) )
    {
      return;
    }

    controller.createNewProject();
  }
}
