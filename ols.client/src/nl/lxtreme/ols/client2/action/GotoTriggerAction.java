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
import nl.lxtreme.ols.client2.icons.*;


/**
 * Provides an action that goes to the trigger point in the captured data, if
 * present.
 */
public class GotoTriggerAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "GotoTrigger";

  // CONSTRUCTORS

  /**
   * Creates a new {@link GotoTriggerAction} instance.
   */
  public GotoTriggerAction()
  {
    super( ID );

    putValue( NAME, "Go to Trigger" );
    putValue( SHORT_DESCRIPTION, "Go to trigger moment in diagram" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_GOTO_TRIGGER ) );
    putValue( ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_HOME, 0 ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_0 ) );

    putValue( MENU_NAME, ClientConstants.DIAGRAM_MENU );
    putValue( MENU_ORDER, 4 );
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
    client.gotoTrigger();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() && aClient.getAcquiredData().hasTriggerData() );
  }
}

/* EOF */
