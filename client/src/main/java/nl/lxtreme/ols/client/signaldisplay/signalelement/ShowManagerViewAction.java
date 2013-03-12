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
package nl.lxtreme.ols.client.signaldisplay.signalelement;


import java.awt.Window;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import nl.lxtreme.ols.client.signaldisplay.SignalDiagramController;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel;
import nl.lxtreme.ols.util.swing.SwingComponentUtils;


/**
 * Provides an Swing-action for opening the {@link SignalElementManagerView}
 * dialog.
 */
public class ShowManagerViewAction extends AbstractAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalDiagramController controller;
  private final SignalElementManager elementManager;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ShowManagerViewAction} instance.
   * 
   * @param aController
   *          the signal diagram controller, cannot be <code>null</code>.
   */
  public ShowManagerViewAction( SignalDiagramController aController )
  {
    this( aController, aController.getSignalDiagramModel() );
  }

  /**
   * Creates a new {@link ShowManagerViewAction} instance.
   * 
   * @param aModel
   *          the signal diagram model, cannot be <code>null</code>.
   */
  public ShowManagerViewAction( SignalDiagramController aController, SignalDiagramModel aModel )
  {
    super( "Configure signal groups" );
    this.controller = aController;
    this.elementManager = aModel.getSignalElementManager();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( ActionEvent aEvent )
  {
    final Window parent = SwingComponentUtils.getOwningWindow( aEvent );

    SignalElementModel model = this.elementManager.createSignalElementModelCopy();

    SignalElementManagerView view = new SignalElementManagerView( parent, model );
    if ( view.showDialog() )
    {
      this.elementManager.setSignalElementModel( model );
      
      this.controller.revalidateAll();
    }
  }
}
