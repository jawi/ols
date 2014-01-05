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
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "zoom in" action, that zooms into the diagram with a constant
 * factor.
 */
public class ZoomInAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ZoomIn";

  // CONSTRUCTORS

  /**
   * Creates a new {@link ZoomInAction} instance.
   */
  public ZoomInAction()
  {
    super( ID );

    putValue( NAME, "Zoom in" );
    putValue( SHORT_DESCRIPTION, "Zooms in with a factor" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_ZOOM_IN ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_PLUS ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_I ) );

    putValue( MENU_NAME, ClientConstants.DIAGRAM_MENU );
    putValue( MENU_ORDER, 0 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    client.zoomIn();
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

/* EOF */
