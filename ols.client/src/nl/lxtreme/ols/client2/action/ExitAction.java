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
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an exit action that shuts down the client entirely.
 */
public class ExitAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "Exit";

  // CONSTRUCTORS

  /**
   * Creates a new ExitAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public ExitAction()
  {
    super( ID );

    putValue( NAME, "Quit" );
    putValue( SHORT_DESCRIPTION, "Quit LogicSniffer Client" );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_Q ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_Q ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, Integer.MAX_VALUE );
    putValue( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    client.exit();
  }
}

/* EOF */
