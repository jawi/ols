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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.action;


import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides a managed action that enables or disables the measurement mode.
 */
public class SetMeasurementModeAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "SetMeasurementMode";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SetMeasurementModeAction} instance.
   */
  public SetMeasurementModeAction()
  {
    super( "Measurement mode" );

    putValue( SELECTED_KEY, Boolean.valueOf( getSignalDiagramController().isMeasurementMode() ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final AbstractButton button = ( AbstractButton )aEvent.getSource();
    getSignalDiagramController().setMeasurementMode( button.isSelected() );
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
    setEnabled( getSession().hasData() );
    putValue( SELECTED_KEY, Boolean.valueOf( getSignalDiagramController().isMeasurementMode() ) );
  }

  /**
   * @return a {@link Session} instance, never <code>null</code>.
   */
  private Session getSession()
  {
    return Client.getInstance().getSession();
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
