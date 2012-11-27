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
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a "zoom original" action, zooming the diagram to the "original"
 * level.
 */
public class ZoomOriginalAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ZoomDefault";

  // CONSTRUCTORS

  /**
   * Creates a new {@link ZoomOriginalAction} instance.
   */
  public ZoomOriginalAction()
  {
    putValue( NAME, "Zoom original" );
    putValue( SHORT_DESCRIPTION, "Zoom to original level" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_ZOOM_DEFAULT ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_0 ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_R ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    getZoomController().zoomDefault();
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
    boolean isZoomOriginal = getZoomController().isZoomDefault();
    setEnabled( dataAvailable && !isZoomOriginal );
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
