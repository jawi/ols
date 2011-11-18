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


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.diagram.laf.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a "set cursor N" action.
 */
public class SetCursorAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "SetCursor";

  // VARIABLES

  private final int cursorIdx;

  // CONSTRUCTORS

  /**
   * Creates a new SetCursorAction instance.
   * 
   * @param aController
   *          the controller to use for this action;
   * @param aCursorIdx
   *          the index of the cursor to set with this action, >= 0 && < 10.
   */
  public SetCursorAction( final ClientController aController, final int aCursorIdx )
  {
    super( getCursorId( aCursorIdx ), aController, "Set Cursor " + ( aCursorIdx + 1 ), "Set the "
        + DisplayUtils.getOrdinalNumber( aCursorIdx + 1 ) + " cursor" );
    this.cursorIdx = aCursorIdx;
  }

  // METHODS

  /**
   * Returns the action ID for the given cursor index.
   * 
   * @param aCursorIdx
   *          the cursor index, >= 0 && < 10.
   * @return a "set cursor" action ID, never <code>null</code>.
   */
  public static final String getCursorId( final int aCursorIdx )
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx >= Ols.MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index: " + aCursorIdx );
    }
    return String.format( "%s.%d", ID, Integer.valueOf( aCursorIdx ) );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final JCheckBoxMenuItem menuitem = ( JCheckBoxMenuItem )aEvent.getSource();

    final Point location = getContextMenuLocation( menuitem );
    if ( menuitem.isSelected() )
    {
      getController().setCursorPosition( this.cursorIdx, location );
    }
    else
    {
      getController().removeCursor( this.cursorIdx );
    }
  }

  /**
   * Returns the context menu location client property of the given menu item's
   * popup menu.
   * 
   * @param aMenuItem
   *          the menu item to return the client property of, cannot be
   *          <code>null</code>.
   * @return a location denoting the context menu's position, never
   *         <code>null</code>.
   */
  private Point getContextMenuLocation( final JMenuItem aMenuItem )
  {
    final JComponent container = ( JComponent )aMenuItem.getParent();

    Point location = ( Point )container.getClientProperty( DiagramUI.CONTEXTMENU_LOCATION_KEY );
    if ( location == null )
    {
      // Make sure we return a defined point...
      return new Point( 0, 0 );
    }

    return location;
  }
}

/* EOF */
