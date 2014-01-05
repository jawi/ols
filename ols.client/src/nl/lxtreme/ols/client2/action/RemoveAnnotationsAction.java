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
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Removes all current annotations from the current signal display.
 */
public class RemoveAnnotationsAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "RemoveAnnotations";

  // CONSTRUCTORS

  /**
   * Creates a new {@link RemoveAnnotationsAction} instance.
   */
  public RemoveAnnotationsAction()
  {
    super( ID );

    putValue( NAME, "Remove annotations" );
    putValue( SHORT_DESCRIPTION, "Removes all existing annotations from the signal display." );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createKeyMask( KeyEvent.VK_DELETE, InputEvent.SHIFT_DOWN_MASK ) );

    putValue( MENU_NAME, ClientConstants.DIAGRAM_MENU );
    putValue( MENU_ORDER, 11 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    client.clearAnnotations();
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() );
  }
}
