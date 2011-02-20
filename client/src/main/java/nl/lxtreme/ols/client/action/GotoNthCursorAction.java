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
import nl.lxtreme.ols.util.swing.*;
import static nl.lxtreme.ols.client.icons.IconFactory.*;

public class GotoNthCursorAction extends BaseAction
{

  private static final long serialVersionUID = 1L;

  public final String ID;
  public final int i;

  public GotoNthCursorAction( final ClientController aController, int n )
  {
    super( "GotoCursor" + String.valueOf( n ), aController, createOverlayIcon( ICON_GOTO_CURSOR, String.valueOf( n ) ),
            "Go to cursor " + String.valueOf( n ), "Go to cursor " + String.valueOf( n ) + " in diagram" );
    this.ID = "GotoCursor" + String.valueOf( n );
    this.i = n - 1; // cursors are 0-based

    if ( n < 0 )
      throw new IllegalArgumentException( "There are no cursors < 0!" );

    if ( n <= 9 )
    {
      int event = KeyEvent.VK_0 + n;
      putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( event ) );
      putValue( MNEMONIC_KEY, new Integer( event ) );

    }
    else if ( n == 10 )
      putValue( MNEMONIC_KEY, KeyEvent.VK_0 ); // ctrl+0 is used for zoom to original, but the mnemonic is free
  }

  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getController().gotoCursorPosition( this.i );
  }
}