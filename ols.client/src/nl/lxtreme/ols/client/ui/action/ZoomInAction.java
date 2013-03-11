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
package nl.lxtreme.ols.client.ui.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.icons.*;
import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "zoom in" action, that zooms into the diagram with a constant
 * factor.
 */
public class ZoomInAction extends AbstractAction implements IManagedAction
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
    putValue( NAME, "Zoom in" );
    putValue( SHORT_DESCRIPTION, "Zooms in with a factor" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_ZOOM_IN ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_PLUS ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_I ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getZoomController().zoomIn();
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
    boolean dataAvailable = hasData();
    boolean canZoomIn = getZoomController().canZoomIn();
    setEnabled( dataAvailable && canZoomIn );
  }

  /**
   * @return the signal diagram's zoom controller, never <code>null</code>.
   */
  private ZoomController getZoomController()
  {
    final SignalDiagramController controller = Client.getInstance().getSignalDiagramController();
    return controller.getZoomController();
  }

  /**
   * @return <code>true</code> if there's data available to display,
   *         <code>false</code> otherwise.
   */
  private boolean hasData()
  {
    final Session session = Client.getInstance().getSession();
    return session.hasData();
  }
}
