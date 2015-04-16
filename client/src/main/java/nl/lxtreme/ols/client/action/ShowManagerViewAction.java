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


import java.awt.*;
import java.awt.event.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an Swing-action for opening the {@link SignalElementManagerView}
 * dialog.
 */
public class ShowManagerViewAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "ShowManagerViewAction";

  private static final String NAME = "Configure signal groups";
  private static final String DESC = "Add or edit signal groups.";

  // VARIABLES

  private final SignalDiagramController controller;
  private final SignalElementManager elementManager;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ShowManagerViewAction} instance.
   * 
   * @param aController
   *          the client controller to use, cannot be <code>null</code>.
   */
  public ShowManagerViewAction( ClientController aController )
  {
    super( ID, aController, NAME, DESC );
    this.controller = aController.getSignalDiagramController();
    this.elementManager = this.controller.getViewModel().getSignalElementManager();
  }

  /**
   * Creates a new {@link ShowManagerViewAction} instance.
   * 
   * @param aController
   *          the signal diagram controller to use, cannot be <code>null</code>.
   */
  public ShowManagerViewAction( SignalDiagramController aController )
  {
    super( ID, null, NAME, DESC );
    this.controller = aController;
    this.elementManager = this.controller.getViewModel().getSignalElementManager();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( ActionEvent aEvent )
  {
    final Window parent = SwingComponentUtils.getOwningWindow( aEvent );

    SignalElementManagerView view = new SignalElementManagerView( parent, this.elementManager );
    if ( view.showDialog() )
    {
      this.controller.revalidateAll();
    }
  }
}
