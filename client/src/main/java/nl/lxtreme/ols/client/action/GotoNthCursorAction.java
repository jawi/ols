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


import static nl.lxtreme.ols.api.data.CapturedData.*;
import static nl.lxtreme.ols.client.icons.IconFactory.*;

import java.awt.event.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Generic action to go to a specific set cursor in the diagram.
 * 
 * @author stefant
 */
public class GotoNthCursorAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID_PREFIX = "GotoCursor";

  // VARIABLES

  private final int index;

  // METHODS

  /**
   * Creates a new GotoNthCursorAction instance.
   * 
   * @param aController
   *          the controller to use for this action;
   * @param aIndex
   *          the (zero-based) index of the cursor.
   */
  public GotoNthCursorAction( final IClientController aController, final int aIndex )
  {
    super( getID( aIndex ), aController, createOverlayIcon( ICON_GOTO_CURSOR, String.valueOf( aIndex + 1 ) ),
        "Go to cursor " + String.valueOf( aIndex + 1 ), "Go to the " + DisplayUtils.getOrdinalNumber( aIndex + 1 )
            + " cursor in the diagram" );
    this.index = aIndex;

    int keyStroke = KeyEvent.VK_0 + ( ( aIndex + 1 ) % MAX_CURSORS );
    if ( keyStroke != KeyEvent.VK_0 )
    {
      // Avoid overwriting CTRL/CMD + 0 as accelerator...
      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( keyStroke ) );
    }
    putValue( MNEMONIC_KEY, Integer.valueOf( keyStroke ) );
  }

  // METHODS

  /**
   * Creates an ID for the action to go the cursor with the given index.
   * 
   * @param aCursorIdx
   *          the cursor index to get the action-ID for, >= 0 && <
   *          {@value CapturedData#MAX_CURSORS}.
   * @return the action ID for the action, never <code>null</code>.
   */
  public static String getID( final int aCursorIdx )
  {
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx >= MAX_CURSORS ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index, should be between 0 and " + MAX_CURSORS );
    }
    return ID_PREFIX + aCursorIdx;
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getController().gotoCursorPosition( this.index );
  }
}
