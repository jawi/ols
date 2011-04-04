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
package nl.lxtreme.ols.client.diagram;


import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.diagram.laf.*;
import nl.lxtreme.ols.client.diagram.settings.*;


/**
 * @author jajans
 */
public class DiagramCorner extends JComponent
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Diagram diagram;

  // CONSTRUCTORS

  /**
   * 
   */
  public DiagramCorner( final Diagram aDiagram )
  {
    this.diagram = aDiagram;

    setOpaque( true );

    // Make sure to properly initialize our (custom) UI...
    updateUI();
  }

  // METHODS

  /**
   * @return
   */
  public final DataContainer getDataContainer()
  {
    return this.diagram.getDataContainer();
  }

  /**
   * @return the diagram
   */
  public final DiagramSettings getDiagramSettings()
  {
    return this.diagram.getDiagramSettings();
  }

  /**
   * Overridden in order to set a custom UI, which not only paints this diagram,
   * but also can be used to manage the various settings, such as colors,
   * height, and so on.
   * 
   * @see javax.swing.JComponent#updateUI()
   */
  @Override
  public void updateUI()
  {
    setUI( new DiagramCornerUI() );
  }
}
