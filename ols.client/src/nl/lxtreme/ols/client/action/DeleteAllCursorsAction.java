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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a UI-action to delete all cursors in a single go.
 */
public class DeleteAllCursorsAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "DeleteAllCursors";

  // CONSTRUCTORS

  /**
   * Creates a new {@link DeleteAllCursorAction} instance.
   */
  public DeleteAllCursorsAction()
  {
    super( "Delete all cursors" );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createKeyMask( KeyEvent.VK_DELETE, InputEvent.SHIFT_DOWN_MASK ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
    {
      getCursorController().removeCursor( i );
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
    final CursorController controller = getCursorController();

    Cursor[] cursors = controller.getDefinedCursors();
    setEnabled( controller.isCursorsVisible() && ( cursors.length > 0 ) );
  }

  /**
   * @return a {@link CursorController} instance, never <code>null</code>.
   */
  private CursorController getCursorController()
  {
    return Client.getInstance().getCursorController();
  }
}
