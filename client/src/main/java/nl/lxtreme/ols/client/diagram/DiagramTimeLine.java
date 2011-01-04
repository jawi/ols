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
package nl.lxtreme.ols.client.diagram;


import java.awt.*;
import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.diagram.laf.*;


/**
 * Provides a component that provides a timeline.
 */
public class DiagramTimeLine extends JComponent implements Scrollable, DiagramCursorChangeListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final Diagram diagram;

  // CONSTRUCTORS

  /**
   * Creates a new DiagramTimeLine instance.
   * 
   * @param aDiagram
   *          the diagram this timeline belongs to, cannot be <code>null</code>.
   */
  public DiagramTimeLine( final Diagram aDiagram )
  {
    super();

    this.diagram = aDiagram;

    // Make sure to properly initialize our (custom) UI...
    updateUI();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.client.diagram.DiagramCursorChangeListener#cursorChanged(int,
   *      int)
   */
  @Override
  public void cursorChanged( final int aCursorIdx, final int aMousePos )
  {
    repaint();
  }

  /**
   * @see nl.lxtreme.ols.client.diagram.DiagramCursorChangeListener#cursorRemoved(int)
   */
  @Override
  public void cursorRemoved( final int aCursorIdx )
  {
    repaint();
  }

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
   * Returns the scale.
   * 
   * @return the scale, never <code>null</code>.
   */
  public final double getScale()
  {
    return this.diagram.getScale();
  }

  /**
   * @see javax.swing.Scrollable#getScrollableBlockIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableBlockIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.VERTICAL )
    {
      return 0;
    }

    return aVisibleRect.width - DiagramTimeLineUI.TIMELINE_INCREMENT;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportHeight()
   */
  @Override
  public boolean getScrollableTracksViewportHeight()
  {
    return true;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportWidth()
   */
  @Override
  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableUnitIncrement(java.awt.Rectangle,
   *      int, int)
   */
  @Override
  public int getScrollableUnitIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    if ( aOrientation == SwingConstants.VERTICAL )
    {
      return 0;
    }

    int currentPosition = aVisibleRect.x;
    int maxUnitIncrement = DiagramTimeLineUI.TIMELINE_INCREMENT;

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
    setUI( new DiagramTimeLineUI() );
  }
}

/* EOF */
