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

import javax.swing.*;

import nl.lxtreme.ols.client2.*;


/**
 * Provides an action that either shows or hides all set cursors.
 */
public class SetCursorsVisibleAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "SetCursorMode";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetCursorsVisibleAction} instance.
   */
  public SetCursorsVisibleAction()
  {
    super( ID );

    putValue( NAME, "Cursors visible" );
    putValue( SHORT_DESCRIPTION, "Toggle visibility of all cursors in the diagram" );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_C ) );

    putValue( MENU_NAME, ClientConstants.DIAGRAM_MENU );
    putValue( MENU_CHECKBOX, Boolean.TRUE );
    putValue( MENU_ORDER, 5 );
    putValue( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    AbstractButton button = ( AbstractButton )aEvent.getSource();

    Client client = getClient( aEvent );
    client.setCursorsVisible( button.isSelected() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    boolean selected = aClient.areCursorsVisible();
    putValue( SELECTED_KEY, Boolean.valueOf( selected ) );

    setEnabled( aClient.hasAcquiredData() );
  }
}

/* EOF */
