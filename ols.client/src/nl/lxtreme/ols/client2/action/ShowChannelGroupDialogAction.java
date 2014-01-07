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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;

import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client2.*;


/**
 * Provides an Swing-action for opening the {@link SignalElementManagerView}
 * dialog.
 */
public class ShowChannelGroupDialogAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ShowManagerViewAction";

  // CONSTRUCTORS

  /**
   * Creates a new {@link ShowChannelGroupDialogAction} instance.
   */
  public ShowChannelGroupDialogAction()
  {
    super( ID );

    putValue( NAME, "Configure channel groups" );
    putValue( SHORT_DESCRIPTION, "Add or edit channel groups." );

    putValue( MENU_NAME, ClientConstants.DIAGRAM_MENU );
    putValue( MENU_ORDER, 12 );
    putValue( MENU_SEPARATOR_ABOVE, Boolean.TRUE );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    client.showChannelGroupManagerDialog( client );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() );
  }
}
