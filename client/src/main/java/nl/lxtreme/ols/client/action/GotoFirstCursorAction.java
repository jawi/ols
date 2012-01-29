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

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.client.signaldisplay.*;


/**
 * Provides an action that goes to the first available cursor.
 */
public class GotoFirstCursorAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "GotoFirstCursor";

  // VARIABLES

  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new GotoFirstCursorAction instance.
   * 
   * @param aController
   *          the signal diagram controller to use for this action.
   */
  public GotoFirstCursorAction( final SignalDiagramController aController )
  {
    this.controller = aController;

    putValue( NAME, "Go to lowest cursor" );
    putValue( SHORT_DESCRIPTION, "Go to the cursor with lowest index in diagram" );
    putValue( Action.LARGE_ICON_KEY, IconLocator.ICON_GOTO_FIRST_CURSOR );

    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_F ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Cursor[] definedCursors = this.controller.getDefinedCursors();
    if ( definedCursors.length > 0 )
    {
      this.controller.scrollToTimestamp( definedCursors[0].getTimestamp() );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return ID;
  }
}

/* EOF */
