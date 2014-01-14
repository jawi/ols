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

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "new project" action.
 */
public class NewProjectAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "NewProject";

  // CONSTRUCTORS

  /**
   * Creates a new {@link NewProjectAction} instance.
   */
  public NewProjectAction()
  {
    super( ID );

    putValue( NAME, "New ..." );
    putValue( SHORT_DESCRIPTION, "Create a new project" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_NEW_PROJECT ) );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_N ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_N ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 0 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    // Issue #62: in case the user does NOT confirm to lose its changes, we
    // should bail out immediately, otherwise continue normally...
    if ( client.isChanged() && //
        !SwingComponentUtils.askConfirmation( client,
            "Current project has been changed.\nDo you really want to lose your changes?" ) )
    {
      return;
    }

    client.newProject();
  }
}
