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
package nl.lxtreme.ols.client.ui.signaldisplay;


import java.awt.*;
import java.util.*;

import javax.swing.event.*;


/**
 * Defines a zoom factor, with a ratio and some additional properties.
 */
public final class ZoomController
{
  // INNER TYPES

  public static final class ZoomEvent
  {
    private final double oldFactor;
    private final double newFactor;
    private final ZoomValue value;
    private final Point centerPoint;

    /**
     * Creates a new ZoomController.ZoomEvent instance.
     */
    public ZoomEvent( final double aOldFactor, final double aNewFactor, final ZoomValue aValue, final Point aCenterPoint )
    {
      this.oldFactor = aOldFactor;
      this.newFactor = aNewFactor;
      this.value = aValue;
      this.centerPoint = aCenterPoint;
    }

    /**
     * Returns the current value of center point.
     * 
     * @return the center point of the zoom action, never <code>null</code>.
     */
    public Point getCenterPoint()
    {
      return this.centerPoint;
    }

    /**
     * Returns the <em>relative</em> zoom factor.
     * 
     * @return the relative zoom factor, >= 0.0.
     */
    public double getFactor()
    {
      return this.newFactor / this.oldFactor;
    }

    /**
     * @return <code>true</code> if this event is fired due to zooming in or
     *         out, <code>false</code> otherwise.
     */
    public boolean isZoomInOrOut()
    {
      return ( ZoomValue.IN == this.value ) || ( ZoomValue.OUT == this.value );
    }
  }

  /**
   * Provides an interface for interchanging zooming events.
   */
  public static interface ZoomListener extends EventListener
  {
    // METHODS

    /**
     * Called upon each change of zoom factor.
     * 
     * @param aEvent
     *          the zoom event details.
     */
    void notifyZoomChange( ZoomEvent aEvent );
  }

  /**
   * Denotes the value with which zooming in/out should happen.
   */
  public static enum ZoomValue
  {
    // CONSTANTS

    /** Zooms in with a constant factor. */
    IN,
    /** Zooms out with a constant factor. */
    OUT,
    /** Zooms to a default level. */
    DEFAULT,
    /** Zooms to a least possible zoom level, showing everything in one view. */
    ALL,
    /** Zooms to a maximum possible zoom level, showing the most detailed view. */
    MAXIMUM;
  }

  // CONSTANTS

  /** The default/original zoom factor. */
  private static final double DEFAULT_ZOOM_FACTOR = 1.0;
  /** The zoom-ratio to use when zooming in (or out, if you use the inverse). */
  private static final double DEFAULT_ZOOM_RATIO = 2.0;

  /**
   * This is what the width of the view component can be at maximum. Swing is
   * entirely based 32-bit signed values, meaning that the theoretical width
   * could be {@link Integer#MAX_VALUE}. However, due the use signed integers,
   * most calculations done with a component of with {@link Integer#MAX_VALUE}
   * will cause an "overflow" and make the values wrap around to negative
   * values. This is especially done when determining whether or not values are
   * in the clipping region of the component. To overcome this, we zero out the
   * lowest 16-bits in order to have enough head-room for integer calculations
   * without "overflowing".
   */
  static final int MAX_COMP_WIDTH = 0x7fff0000;

  // VARIABLES

  private final SignalDiagramController controller;
  private final EventListenerList eventListeners;

  private final Object LOCK = new Object();

  private volatile ZoomValue value = ZoomValue.DEFAULT;
  private volatile double factor = DEFAULT_ZOOM_FACTOR;

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
    this.eventListeners = new EventListenerList();
  }

  // METHODS

  /**
   * Adds a given zoom listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addZoomListener( final ZoomListener aListener )
  {
    this.eventListeners.add( ZoomListener.class, aListener );
  }

  /**
   * Returns whether or not we can zoom in further.
   * 
   * @return <code>true</code> if we can zoom in, <code>false</code> if the
   *         maximum zoom level has been reached.
   */
  public boolean canZoomIn()
  {
    final double maxZoomLevel = getMaxZoomLevel();
    final double zoomFactor = getFactor();
    return zoomFactor < maxZoomLevel;
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
    final double zoomFactor = getFactor();
    return zoomFactor > minZoomLevel;
  }

  /**
   * Returns the current value of factor.
   * 
   * @return the factor
   */
  public double getFactor()
  {
    final double result;
    synchronized ( this.LOCK )
    {
      result = Double.isNaN( this.factor ) ? DEFAULT_ZOOM_FACTOR : this.factor;
    }
    return result;
  }

  /**
   * Returns whether or not we're zooming to fit all.
   * 
   * @return <code>true</code> if zoom-all is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isZoomAll()
  {
    synchronized ( this.LOCK )
    {
      return ( this.value == ZoomValue.ALL );
    }
  }

  /**
   * @return <code>true</code> if the default zoom level is selected,
   *         <code>false</code> otherwise.
   */
  public boolean isZoomDefault()
  {
    synchronized ( this.LOCK )
    {
      return ( this.value == ZoomValue.DEFAULT );
    }
  }

  /**
   * Removes a given zoom listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeZoomListener( final ZoomListener aListener )
  {
    this.eventListeners.remove( ZoomListener.class, aListener );
  }

  /**
   * Restores the zoom-level to the current level, and notifies all listeners.
   */
  public void restoreZoomLevel()
  {
    ZoomValue _value;
    double _factor;
    synchronized ( this.LOCK )
    {
      _value = this.value;
      _factor = this.factor;
    }

    zoom( _value, _factor, null );
  }

  /**
   * Zooms to make the entire view visible.
   */
  public void zoomAll()
  {
    zoom( ZoomValue.ALL, getMinZoomLevel(), null );
  }

  /**
   * Zooms to the default zoom level.
   */
  public void zoomDefault()
  {
    zoom( ZoomValue.DEFAULT, getDefaultZoomLevel(), null );
  }

  /**
   * Zooms in with a constant factor.
   * 
   * @param aCenterPoint
   *          the center point for the zoom action, may be <code>null</code>.
   */
  public void zoomIn( final Point aCenterPoint )
  {
    zoom( ZoomValue.IN, DEFAULT_ZOOM_RATIO, aCenterPoint );
  }

  /**
   * Zooms to make the most detailed view possible.
   */
  public void zoomMaximum()
  {
    zoom( ZoomValue.ALL, getMaxZoomLevel(), null );
  }

  /**
   * Zooms out with a constant factor.
   * 
   * @param aCenterPoint
   *          the center point for the zoom action, may be <code>null</code>.
   */
  public void zoomOut( final Point aCenterPoint )
  {
    zoom( ZoomValue.OUT, 1.0 / DEFAULT_ZOOM_RATIO, aCenterPoint );
  }

  /**
   * @param aPoint1
   * @param aPoint2
   */
  public boolean zoomRegion( final Point aPoint1, final Point aPoint2 )
  {
    if ( aPoint1.distance( aPoint2 ) < 10 ) // XXX threshold!
    {
      return false;
    }

    // Zoom region...
    // @formatter:off
/*
    final SignalDiagramModel model = this.controller.getSignalDiagramModel();
    final Rectangle viewSize = getSignalDiagram().getOuterViewSize();

    final int width = Math.abs( aPoint2.x - aPoint1.x );
    final Long triggerPos = model.getTriggerPosition();

    long ts;
    synchronized ( this.LOCK )
    {
      double oldFactor = getFactor();

      int midX = ( int )( aPoint1.x + ( width / 2.0 ) );

      ts = ( long )Math.ceil( midX / oldFactor );
      if ( triggerPos != null )
      {
        ts -= triggerPos.longValue();
      }

      Point hs = new Point( ( int )( ts * oldFactor ), 0 );
      double newFactor = ( viewSize.width / ( double )width );
    }

    fireZoomEvent();
*/
    // @formatter:on
    return true;
  }

  /**
   * Fires an event to all interested listeners that the zoom level has changed.
   */
  private void fireZoomEvent( final ZoomEvent aEvent )
  {
    ZoomListener[] listeners = this.eventListeners.getListeners( ZoomListener.class );
    for ( ZoomListener listener : listeners )
    {
      listener.notifyZoomChange( aEvent );
    }
  }

  /**
   * @return the default zoom level.
   */
  private double getDefaultZoomLevel()
  {
    double minLevel = getMinZoomLevel();
    if ( minLevel > DEFAULT_ZOOM_FACTOR )
    {
      return minLevel;
    }
    return DEFAULT_ZOOM_FACTOR;
  }

  /**
   * Determines the maximum zoom level that we can handle without causing
   * display problems.
   * <p>
   * It appears that the maximum width of a component can be
   * {@link Integer#MAX_VALUE} pixels wide.
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
    final double max = Integer.MAX_VALUE / MAX_COMP_WIDTH;

    return Math.max( max, Integer.MAX_VALUE / length );
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

    final double width = getVisibleViewSize().getWidth();
    final double length = model.getAbsoluteLength();
    final double min = 1.0 / MAX_COMP_WIDTH;

    return Math.max( min, width / length );
  }

  /**
   * @return the signal diagram model, never <code>null</code>.
   */
  private SignalDiagramModel getModel()
  {
    return this.controller.getSignalDiagramModel();
  }

  /**
   * @return
   */
  private SignalDiagramComponent getSignalDiagram()
  {
    return this.controller.getSignalDiagram();
  }

  /**
   * @return the view size of the current view, never <code>null</code>.
   */
  private Rectangle getVisibleViewSize()
  {
    return getSignalDiagram().getOuterViewSize();
  }

  /**
   * Zooms in with a factor 2.
   */
  private void zoom( final ZoomValue aZoomValue, final double aFactor, final Point aCenterPoint )
  {
    double oldFactor = getFactor();
    double newFactor = aFactor; // assume the factor is absolute...
    ZoomValue newValue = aZoomValue;

    // Ensure the zoom level is always bounded to the current view and the
    // number of samples...
    final Rectangle visibleViewSize = getVisibleViewSize();
    final SignalDiagramModel model = getModel();

    double currentViewWidth = visibleViewSize.getWidth();
    double absLength = model.getAbsoluteLength();

    // Make sure less samples do not cause empty bars on the screen...
    double oldViewWidth = Math.round( absLength * oldFactor );

    if ( ( aZoomValue == ZoomValue.IN ) || ( aZoomValue == ZoomValue.OUT ) )
    {
      // The given factor is relative...
      newFactor = aFactor * oldFactor;
      newValue = null;
    }
    else if ( aZoomValue == ZoomValue.ALL )
    {
      newFactor = currentViewWidth / absLength;
    }

    if ( oldViewWidth < currentViewWidth )
    {
      newFactor = currentViewWidth / absLength;
    }

    Point centerPoint = aCenterPoint;
    if ( centerPoint == null )
    {
      Rectangle dims = getSignalDiagram().getVisibleRect();
      centerPoint = new Point( ( int )dims.getCenterX(), 0 );
    }

    double defaultZoomLevel = getDefaultZoomLevel();
    double minZoomLevel = getMinZoomLevel();
    double maxZoomLevel = getMaxZoomLevel();

    if ( Math.abs( newFactor - defaultZoomLevel ) < 1.0e-6 )
    {
      newFactor = defaultZoomLevel;
      newValue = ZoomValue.DEFAULT;
    }

    if ( Double.compare( newFactor, minZoomLevel ) <= 0.0 )
    {
      newFactor = minZoomLevel;
      newValue = ZoomValue.ALL;
    }
    else if ( Double.compare( newFactor, maxZoomLevel ) >= 0.0 )
    {
      newFactor = maxZoomLevel;
      newValue = ZoomValue.MAXIMUM;
    }

    synchronized ( this.LOCK )
    {
      this.factor = newFactor;
      this.value = newValue;
    }

    fireZoomEvent( new ZoomEvent( oldFactor, newFactor, aZoomValue, centerPoint ) );
  }
}
