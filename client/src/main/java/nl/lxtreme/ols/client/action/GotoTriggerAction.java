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

import javax.swing.*;

import nl.lxtreme.ols.client.*;


/**
 * Provides an action that goes to the trigger point in the captured data, if
 * present.
 */
public class GotoTriggerAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "GotoTrigger";

  // CONSTRUCTORS

  /**
   * Creates a new GotoTriggerAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public GotoTriggerAction( final ClientController aController )
  {
    super( ID, aController, ICON_GOTO_TRIGGER, "Go to Trigger", "Go to trigger moment in diagram" );
    putValue( ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_HOME, 0 ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_0 ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getController().gotoTriggerPosition();
  }
}

/* EOF */
