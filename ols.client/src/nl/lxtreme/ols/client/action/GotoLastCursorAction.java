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
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides an action that goes to the last available cursor.
 */
public class GotoLastCursorAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "GotoLastCursor";

  // CONSTRUCTORS

  /**
   * Creates a new {@link GotoLastCursorAction} instance.
   */
  public GotoLastCursorAction()
  {
    putValue( NAME, "Go to highest cursor" );
    putValue( SHORT_DESCRIPTION, "Go to the cursor with highest index in diagram" );
    putValue( LARGE_ICON_KEY, IconLocator.ICON_GOTO_LAST_CURSOR );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_L ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Cursor[] definedCursors = getCursorController().getDefinedCursors();
    if ( definedCursors.length > 0 )
    {
      Long timestamp = Long.valueOf( definedCursors[definedCursors.length - 1].getTimestamp() );
      getSignalDiagramController().scrollToTimestamp( timestamp );
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

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState()
  {
    Cursor[] cursors = getCursorController().getDefinedCursors();
    setEnabled( cursors.length > 0 );
  }

  /**
   * @return a {@link CursorController} instance, never <code>null</code>.
   */
  private CursorController getCursorController()
  {
    return Client.getInstance().getCursorController();
  }

  /**
   * @return a {@link SignalDiagramController} instance, never <code>null</code>
   *         .
   */
  private SignalDiagramController getSignalDiagramController()
  {
    return Client.getInstance().getSignalDiagramController();
  }
}

/* EOF */
