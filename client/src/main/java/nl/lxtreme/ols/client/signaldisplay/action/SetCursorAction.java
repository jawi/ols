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
package nl.lxtreme.ols.client.signaldisplay.action;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an action to set a cursor on a certain location.
 */
public class SetCursorAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String KEY = "SetCursorAction";

  // VARIABLES

  private final SignalDiagramController controller;
  private final int cursorIdx;

  // CONSTRUCTORS

  /**
   * Creates a new SetCursorAction instance.
   */
  public SetCursorAction( final SignalDiagramController aController, final int aCursorIdx )
  {
    super( "Set cursor " + ( aCursorIdx + 1 ) );
    this.controller = aController;
    this.cursorIdx = aCursorIdx;

    int keyStroke = KeyEvent.VK_0 + ( ( aCursorIdx + 1 ) % Ols.MAX_CURSORS );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createKeyMask( keyStroke ) );

    putValue( Action.SELECTED_KEY, Boolean.valueOf( this.controller.isCursorDefined( aCursorIdx ) ) );
  }

  // METHODS

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
  private static Point getContextMenuLocation( final JMenuItem aMenuItem )
  {
    final JComponent container = ( JComponent )aMenuItem.getParent();

    Point location = ( Point )container.getClientProperty( KEY );
    if ( location == null )
    {
      // Make sure we return a defined point...
      return new Point( 0, 0 );
    }

    return location;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final JMenuItem menuitem = ( JMenuItem )aEvent.getSource();

    // Implicitly enable the cursor mode when a cursor is defined...
    if ( !this.controller.isCursorMode() )
    {
      this.controller.setCursorMode( true );
    }

    final Point location = getContextMenuLocation( menuitem );
    this.controller.moveCursor( this.cursorIdx, location );
  }
}
