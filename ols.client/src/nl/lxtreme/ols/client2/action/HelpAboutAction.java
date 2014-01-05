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


/**
 * Shows some information about this client (version stuff, legal stuff, and
 * such).
 */
public class HelpAboutAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "HelpAbout";

  // CONSTRUCTORS

  /**
   * Creates a new {@link HelpAboutAction} instance.
   */
  public HelpAboutAction()
  {
    super( ID );

    putValue( NAME, "About OLS" );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_A ) );

    putValue( MENU_NAME, ClientConstants.HELP_MENU );
    putValue( MENU_ORDER, 1 );
    putValue( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    client.showAboutBox( client );
  }
}
