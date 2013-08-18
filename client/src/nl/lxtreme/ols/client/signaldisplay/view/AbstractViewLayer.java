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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay.view;


import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.dnd.*;


/**
 * Provides a common base class for view components.
 */
abstract class AbstractViewLayer extends JComponent
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new AbstractViewLayer instance.
   */
  public AbstractViewLayer( final SignalDiagramController aController )
  {
    this.controller = aController;
  }

  // METHODS

  /**
   * Returns the drag-and-drop target controller of this component.
   * 
   * @return a DnD target controller, never <code>null</code>.
   */
  public final DragAndDropTargetController getDnDTargetController()
  {
    return this.controller.getDndTargetController();
  }

  /**
   * Returns the {@link SignalDiagramController} instance.
   * 
   * @return a {@link SignalDiagramController} instance, never <code>null</code>
   *         .
   */
  protected final SignalDiagramController getController()
  {
    return this.controller;
  }
}
