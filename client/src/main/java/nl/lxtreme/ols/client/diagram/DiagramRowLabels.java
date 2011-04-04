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


import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.diagram.laf.*;


/**
 * Provides a component that displays the row headers/labels.
 */
public class DiagramRowLabels extends JComponent implements Scrollable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Diagram diagram;

  // CONSTRUCTORS

  /**
   * Creates a new DiagramRowLabels instance.
   * 
   * @param aDiagram
   *          the diagram these rowlabels belong to, cannot be <code>null</code>
   *          .
   */
  public DiagramRowLabels( final Diagram aDiagram )
  {
    super();

    this.diagram = aDiagram;

    // Make sure to properly initialize our (custom) UI...
    updateUI();
  }

  // METHODS

  /**
   * @return the dataContainer
   */
  public final DataContainer getDataContainer()
  {
    return this.diagram.getDataContainer();
  }

  /**
   * @return the diagram
   */
  public final Diagram getDiagram()
  {
    return this.diagram;
  }

  /**
   * @see javax.swing.Scrollable#getPreferredScrollableViewportSize()
   */
  @Override
  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }

  /**
   * @see javax.swing.Scrollable#getScrollableBlockIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableBlockIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.HORIZONTAL )
    {
      return 0;
    }

    return aVisibleRect.height - DiagramRowLabelsUI.ROW_INCREMENT;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportHeight()
   */
  @Override
  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportWidth()
   */
  @Override
  public boolean getScrollableTracksViewportWidth()
  {
    return true;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableUnitIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableUnitIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.HORIZONTAL )
    {
      return 0;
    }

    int currentPosition = aVisibleRect.y;
    int maxUnitIncrement = DiagramRowLabelsUI.ROW_INCREMENT;

    // Return the number of pixels between currentPosition
    // and the nearest tick mark in the indicated direction.
    if ( aDirection < 0 )
    {
      int newPosition = currentPosition - ( currentPosition / maxUnitIncrement ) * maxUnitIncrement;
      return ( newPosition == 0 ) ? maxUnitIncrement : newPosition;
    }
    else
    {
      return ( ( currentPosition / maxUnitIncrement ) + 1 ) * maxUnitIncrement - currentPosition;
    }
  }

  /**
   * @see javax.swing.JComponent#updateUI()
   */
  @Override
  public void updateUI()
  {
    setUI( new DiagramRowLabelsUI() );
  }
}

/* EOF */
