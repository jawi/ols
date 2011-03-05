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
package nl.lxtreme.ols.client.action;


import java.awt.event.*;

import nl.lxtreme.ols.client.*;


/**
 * Provides an action that goes to the first available cursor.
 */
public class GotoFirstCursorAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "GotoFirstCursor";

  // CONSTRUCTORS

  /**
   * Creates a new GotoFirstCursorAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public GotoFirstCursorAction( final IClientController aController )
  {
    super( ID, aController, ICON_GOTO_FIRST_CURSOR, "Go to lowest cursor",
        "Go to the cursor with lowest index in diagram" );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_F ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getController().gotoFirstAvailableCursor();
  }
}

/* EOF */
