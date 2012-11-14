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


import static nl.lxtreme.ols.client.icons.IconLocator.*;

import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides an action that goes to the trigger point in the captured data, if
 * present.
 */
public class GotoTriggerAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "GotoTrigger";

  // CONSTRUCTORS

  /**
   * Creates a new {@link GotoTriggerAction} instance.
   */
  public GotoTriggerAction()
  {
    putValue( NAME, "Go to Trigger" );
    putValue( SHORT_DESCRIPTION, "Go to trigger moment in diagram" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_GOTO_TRIGGER ) );

    putValue( ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_HOME, 0 ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_0 ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = Client.getInstance();

    Session session = client.getSession();

    AcquisitionData capturedData = session.getAcquisitionData();
    if ( ( capturedData != null ) && capturedData.hasTriggerData() )
    {
      final long position = capturedData.getTriggerPosition();

      SignalDiagramController controller = client.getSignalDiagramController();
      controller.scrollToTimestamp( Long.valueOf( position ) );
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
    final Session session = Client.getInstance().getSession();

    boolean enabled = false;
    if ( session.hasData() )
    {
      AcquisitionData capturedData = session.getAcquisitionData();
      enabled = capturedData.hasTriggerData();
    }

    setEnabled( enabled );
  }
}

/* EOF */
