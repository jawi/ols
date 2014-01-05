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
 * Copyright (C) 2006-2010 Michael Poppitz, www.sump.org
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.*;


/**
 * Provides a managed action that enables or disables the snapping of cursors.
 */
public class SetCursorSnapModeAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "SetSnapCursorMode";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetCursorSnapModeAction} instance.
   */
  public SetCursorSnapModeAction()
  {
    super( ID );

    putValue( NAME, "Snap cursors" );

    putValue( MENU_NAME, ClientConstants.DIAGRAM_MENU );
    putValue( MENU_CHECKBOX, Boolean.TRUE );
    putValue( MENU_ORDER, 8 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    JCheckBoxMenuItem menuItem = ( JCheckBoxMenuItem )aEvent.getSource();

    Client client = getClient( aEvent );
    client.setCursorSnapMode( menuItem.isSelected() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    boolean selected = aClient.isCursorSnapMode();
    putValue( SELECTED_KEY, Boolean.valueOf( selected ) );
    if ( selected )
    {
      putValue( SHORT_DESCRIPTION, "Snaps the cursors to nearest signal transition." );
    }
    else
    {
      putValue( SHORT_DESCRIPTION, "Cursors can be placed freely." );
    }

    setEnabled( aClient.hasAcquiredData() );
  }
}
