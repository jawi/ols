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
package nl.lxtreme.ols.client.signaldisplay;


import java.awt.*;
import java.util.logging.*;

import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Defines a zoom factor, with a ratio and some additional properties.
 */
public class ZoomController
{
  // INNER TYPES

  /**
   * Denotes a zoom-event.
   */
  public static class ZoomEvent
  {
    // VARIABLES

    private final double minZoomLevel;
    private final double maxZoomLevel;
    private final double factor;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ZoomEvent} instance.
     * 
     * @param aFactor
     * @param aMinZoomLevel
     * @param aMaxZoomLevel
     */
    public ZoomEvent( final double aFactor, final double aMinZoomLevel, final double aMaxZoomLevel )
    {
      this.factor = aFactor;
      this.minZoomLevel = aMinZoomLevel;
      this.maxZoomLevel = aMaxZoomLevel;
    }

    // METHODS

    /**
     * Returns whether or not we can zoom in further.
     * 
     * @return <code>true</code> if we can zoom in, <code>false</code> if the
     *         maximum zoom level has been reached.
     */
    public boolean canZoomIn()
    {
      final double maxZoomLevel = getMaxZoomLevel();
      return getFactor() < maxZoomLevel;
    }

    /**
     * Returns whether or not we can zoom out further.
     * 
     * @return <code>true</code> if we can zoom out, <code>false</code> if the
     *         maximum zoom level has been reached.
     */
    public boolean canZoomOut()
    {
      final double minZoomLevel = getMinZoomLevel();
      return getFactor() > minZoomLevel;
    }

    /**
     * Returns the current value of factor.
     * 
     * @return the factor
     */
    public double getFactor()
    {
      return this.factor;
    }

    /**
     * Returns the current value of maxZoomLevel.
     * 
     * @return the maxZoomLevel
     */
    public double getMaxZoomLevel()
    {
      return this.maxZoomLevel;
    }

    /**
     * Returns the current value of minZoomLevel.
     * 
     * @return the minZoomLevel
     */
    public double getMinZoomLevel()
    {
      return this.minZoomLevel;
    }

    /**
     * Returns whether or not we're zooming to fit all.
     * 
     * @return <code>true</code> if zoom-all is enabled, <code>false</code>
     *         otherwise.
     */
    public boolean isZoomAll()
    {
      return !canZoomOut();
    }

    /**
     * @return <code>true</code> if the default zoom level is selected,
     *         <code>false</code> otherwise.
     */
    public boolean isZoomOriginal()
    {
      return getFactor() == DEFAULT_ZOOM_FACTOR;
    }

  }

  // CONSTANTS

  private static final double DEFAULT_ZOOM_FACTOR = 1.0;

  private static final Logger LOG = Logger.getLogger( ZoomController.class.getName() );

  // VARIABLES

  private boolean zoomAll;
  private final SignalDiagramController controller;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ZoomController} instance.
   * 
   * @param aController
   *          the signal diagram controller to use.
   */
  public ZoomController( final SignalDiagramController aController )
  {
    this.controller = aController;

    // The default...
    this.zoomAll = true;
  }

  // METHODS

  /**
   * Returns the current value of factor.
   * 
   * @return the factor
   */
  public double getFactor()
  {
    return getModel().getZoomFactor();
  }

  /**
   * Returns whether or not we're zooming to fit all.
   * 
   * @return <code>true</code> if zoom-all is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isZoomAll()
  {
    return this.zoomAll;
  }

  /**
   * @return <code>true</code> if the default zoom level is selected,
   *         <code>false</code> otherwise.
   */
  public boolean isZoomOriginal()
  {
    return getFactor() == DEFAULT_ZOOM_FACTOR;
  }

  /**
   * Restores the zoom-level to the current level, and notifies all listeners.
   */
  public void restoreZoomLevel()
  {
    this.controller.notifyZoomChange( new ZoomEvent( getFactor(), getMinZoomLevel(), getMaxZoomLevel() ) );
  }

  /**
   * Zooms to make all data visible in one screen.
   */
  public void zoomAll()
  {
    setFactor( getMinZoomLevel() );
    this.zoomAll = true;

    LOG.log( Level.INFO, "Zoom factor set to " + getFactor() );

    this.controller.notifyZoomChange( new ZoomEvent( getFactor(), getMinZoomLevel(), getMaxZoomLevel() ) );
  }

  /**
   * Zooms in with a factor 1.5
   */
  public void zoomIn()
  {
    zoomRelative( 2.0 );

    this.controller.notifyZoomChange( new ZoomEvent( getFactor(), getMinZoomLevel(), getMaxZoomLevel() ) );
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    zoomAbsolute( DEFAULT_ZOOM_FACTOR );

    this.controller.notifyZoomChange( new ZoomEvent( getFactor(), getMinZoomLevel(), getMaxZoomLevel() ) );
  }

  /**
   * Zooms out with a factor 1.5
   */
  public void zoomOut()
  {
    zoomRelative( 0.5 );

    this.controller.notifyZoomChange( new ZoomEvent( getFactor(), getMinZoomLevel(), getMaxZoomLevel() ) );
  }

  /**
   * Determines the maximum zoom level that we can handle without causing
   * display problems.
   * <p>
   * It appears that the maximum width of a component can be
   * {@link Short#MAX_VALUE} pixels wide.
   * </p>
   * 
   * @return a maximum zoom level.
   */
  private double getMaxZoomLevel()
  {
    final SignalDiagramModel model = getModel();
    if ( !model.hasData() )
    {
      return DEFAULT_ZOOM_FACTOR;
    }

    final double length = model.getAbsoluteLength();
    return Math.floor( Integer.MAX_VALUE / length );
  }

  /**
   * Determines the minimum zoom level that we can causes all signals to be
   * displayed in the current width and height.
   * 
   * @return a minimum zoom level.
   */
  private double getMinZoomLevel()
  {
    final SignalDiagramModel model = getModel();
    if ( !model.hasData() )
    {
      return DEFAULT_ZOOM_FACTOR;
    }

    Rectangle viewSize = this.controller.getSignalDiagram().getVisibleViewSize();
    final double length = model.getAbsoluteLength();

    return viewSize.getWidth() / length;
  }

  /**
   * @return
   */
  private SignalDiagramModel getModel()
  {
    return this.controller.getSignalDiagramModel();
  }

  /**
   * Sets the factor.
   * 
   * @param aFactor
   *          the factor to set
   */
  private void setFactor( final double aFactor )
  {
    getModel().setZoomFactor( aFactor );
  }

  /**
   * @param aFactor
   */
  private void zoomAbsolute( final double aFactor )
  {
    setFactor( aFactor );
    this.zoomAll = false;

    LOG.log( Level.INFO, "Zoom factor set to " + getFactor() );
  }

  /**
   * @param aFactor
   */
  private void zoomRelative( final double aFactor )
  {
    final double newFactor = Math.max( getMinZoomLevel(), Math.min( getMaxZoomLevel(), aFactor * getFactor() ) );
    zoomAbsolute( newFactor );
  }
}
