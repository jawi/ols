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
package nl.lxtreme.ols.client.ui.signaldisplay.view;


import static nl.lxtreme.ols.client.ui.signaldisplay.view.UIManagerKeys.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.marker.*;
import nl.lxtreme.ols.client.ui.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.ui.signaldisplay.signalelement.SignalElementManager.SignalElementMeasurer;


/**
 * Provides a common base class for the view models.
 */
public abstract class AbstractViewModel
{
  // VARIABLES

  protected final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new AbstractViewModel instance.
   * 
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  protected AbstractViewModel( final SignalDiagramController aController )
  {
    this.controller = aController;
  }

  // METHODS

  /**
   * @return an array of defined markers, never <code>null</code>.
   */
  public Marker[] getDefinedMarkers()
  {
    return getSignalDiagramModel().getDefinedMarkers();
  }

  /**
   * @return
   */
  public SignalElementManager getSignalElementManager()
  {
    return getSignalDiagramModel().getSignalElementManager();
  }

  /**
   * Returns all channels the given range of all visible channel groups.
   * 
   * @param aY
   *          the screen Y-coordinate;
   * @param aHeight
   *          the screen height.
   * @return an array of channels, never <code>null</code>.
   */
  public SignalElement[] getSignalElements( final int aY, final int aHeight )
  {
    // Return all channel elements within the given boundaries, even if they do
    // not completely fit...
    return getSignalElementManager().getSignalElements( aY, aHeight, SignalElementMeasurer.LOOSE_MEASURER );
  }

  /**
   * Returns the spacing between two signal elements.
   * 
   * @return a signal element spacing, in pixels.
   */
  public int getSignalElementSpacing()
  {
    return UIManager.getInt( SIGNAL_ELEMENT_SPACING );
  }

  /**
   * Returns the current zoom factor that is used to display the signals with.
   * 
   * @return a zoom factor, >= 0.0.
   */
  public double getZoomFactor()
  {
    return getSignalDiagramModel().getZoomFactor();
  }

  /**
   * Returns whether or not there is captured data to display.
   * 
   * @return <code>true</code> if there is any data to display,
   *         <code>false</code> otherwise.
   */
  public boolean hasData()
  {
    return getSignalDiagramModel().hasData();
  }

  /**
   * @return <code>true</code> if cursors are to be displayed,
   *         <code>false</code> otherwise.
   */
  public boolean isCursorMode()
  {
    return getSignalDiagramModel().isCursorMode();
  }

  /**
   * Converts a given time stamp to a screen coordinate.
   * 
   * @param aTimestamp
   *          the time stamp to convert, >= 0.
   * @return a screen coordinate, >= 0.
   */
  public int timestampToCoordinate( final long aTimestamp )
  {
    double result = getSignalDiagramModel().getZoomFactor() * aTimestamp;
    if ( result > Integer.MAX_VALUE )
    {
      return Integer.MAX_VALUE;
    }
    return ( int )result;
  }

  /**
   * @return the signal diagram model, never <code>null</code>.
   */
  protected final SignalDiagramModel getSignalDiagramModel()
  {
    return this.controller.getSignalDiagramModel();
  }
}
