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
import java.awt.event.MouseWheelEvent;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.event.*;

import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Defines a zoom factor, with a ratio and some additional properties.
 */
public final class ZoomController
{
  // INNER TYPES

  /**
   * Denotes a zooming event.
   */
  public final class ZoomEvent
  {
    // VARIABLES

    private final double oldFactor;
    private final double newFactor;
    private final ZoomValue value;
    private final Point centerPoint;
    private final Dimension dimension;

    // CONSTRUCTORS

    /**
     * Creates a new ZoomController.ZoomEvent instance.
     */
    public ZoomEvent( final double aOldFactor, final double aNewFactor, final ZoomValue aValue,
        final Point aCenterPoint, final Dimension aDimension )
    {
      this.oldFactor = aOldFactor;
      this.newFactor = aNewFactor;
      this.value = aValue;
      this.centerPoint = aCenterPoint;
      this.dimension = aDimension;
    }

    // METHODS

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
     * @return the proposed view dimensions, never <code>null</code>.
     */
    public Dimension getDimension()
    {
      return dimension;
    }

    /**
     * Returns the <em>relative</em> zoom factor in case this event represents a
     * zoom in/out event, or the <em>absolute</em> factor otherwise.
     * 
     * @return the zoom factor, >= 0.0.
     * @see #isZoomInOrOut()
     */
    public double getFactor()
    {
      if ( isZoomInOrOut() )
      {
        return this.newFactor / this.oldFactor;
      }
      return this.newFactor;
    }

    /**
     * @return the current zoom controller, never <code>null</code>.
     */
    public ZoomController getZoomController()
    {
      return ZoomController.this;
    }

    /**
     * @return <code>true</code> if this event is fired due to zooming in or
     *         out, <code>false</code> otherwise.
     */
    public boolean isZoomInOrOut()
    {
      return ( ZoomValue.IN == this.value ) || ( ZoomValue.OUT == this.value );
    }

    @Override
    public String toString()
    {
      return String.format( "ZoomEvent[ZV = %s, old Zf = %f, new Zf = %f, CP = %s, Dim = %s]", value, oldFactor, newFactor,
          centerPoint, dimension );
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
     *          the zoom event with current zoom information, never
     *          <code>null</code>.
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

  private static class ZoomHolder
  {
    final double factor;
    final ZoomValue value;

    public ZoomHolder( ZoomValue aValue, double aFactor )
    {
      this.value = aValue;
      this.factor = aFactor;
    }
  }

  // CONSTANTS

  /** The default/original zoom factor. */
  private static final double DEFAULT_ZOOM_FACTOR = 1.0;
  /** The zoom-ratio to use when zooming in (or out, if you use the inverse). */
  private static final double DEFAULT_ZOOM_RATIO = 2.0;

  // VARIABLES

  private final SignalDiagramController controller;
  private final EventListenerList eventListeners;

  private final AtomicReference<ZoomHolder> zoomHolderRef;

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

    this.zoomHolderRef = new AtomicReference<ZoomHolder>( new ZoomHolder( ZoomValue.DEFAULT, DEFAULT_ZOOM_FACTOR ) );
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
    ZoomHolder zh = this.zoomHolderRef.get();
    double result = zh.factor;
    if ( Double.isNaN( result ) )
    {
      result = DEFAULT_ZOOM_FACTOR;
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
    ZoomHolder zh = this.zoomHolderRef.get();
    ZoomValue value = zh.value;

    return ( value == ZoomValue.ALL );
  }

  /**
   * @return <code>true</code> if the default zoom level is selected,
   *         <code>false</code> otherwise.
   */
  public boolean isZoomDefault()
  {
    ZoomHolder zh = this.zoomHolderRef.get();
    ZoomValue value = zh.value;

    return ( value == ZoomValue.DEFAULT );
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
    ZoomHolder zh = this.zoomHolderRef.get();

    zoom( zh.value, zh.factor, null );
  }

  /**
   * Zooms in or out with a constant factor, according to the given mouse wheel
   * event.
   * 
   * @param aRotation
   *          the mouse wheel rotation, either positive or negative;
   * @param aPoint
   *          the location of the mouse pointer, can be <code>null</code>.
   * @see MouseWheelEvent#getWheelRotation()
   */
  public void zoom( int aRotation, Point aPoint )
  {
    if ( aRotation > 0 )
    {
      double ratio = 1.0 / ( aRotation * DEFAULT_ZOOM_RATIO );
      zoom( ZoomValue.OUT, ratio, aPoint );
    }
    else if ( aRotation < 0 )
    {
      double ratio = ( -aRotation * DEFAULT_ZOOM_RATIO );
      zoom( ZoomValue.IN, ratio, aPoint );
    }
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
   * Zooms in with a constant factor around the current view-center.
   */
  public void zoomIn()
  {
    zoom( ZoomValue.IN, DEFAULT_ZOOM_RATIO, null );
  }

  /**
   * Zooms to make the most detailed view possible.
   */
  public void zoomMaximum()
  {
    zoom( ZoomValue.ALL, getMaxZoomLevel(), null );
  }

  /**
   * Zooms out with a constant factor around the current view-center.
   */
  public void zoomOut()
  {
    zoom( ZoomValue.OUT, 1.0 / DEFAULT_ZOOM_RATIO, null );
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

    return true;
  }

  /**
   * Creates a new ZoomEvent instance, based on the current situation.
   * 
   * @return a new {@link ZoomEvent} instance, never <code>null</code>.
   */
  private ZoomEvent createZoomEvent( ZoomValue aZoomValue, final double aOldFactor, final double aNewFactor,
      final Point aCenterPoint, final Dimension aDimension )
  {
    return new ZoomEvent( aOldFactor, aNewFactor, aZoomValue, aCenterPoint, aDimension );
  }

  /**
   * Fires a given {@link ZoomEvent} to all interested listeners.
   * 
   * @param aEvent
   *          the event to fire, cannot be <code>null</code>.
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

    final double width = getVisibleViewSize().getWidth();
    final double length = model.getAbsoluteLength();

    return width / length;
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
    return getSignalDiagram().getVisibleViewSize();
  }

  /**
   * Zooms in with a factor 2.
   */
  private void zoom( final ZoomValue aZoomValue, final double aFactor, final Point aCenterPoint )
  {
    double oldFactor = getFactor();
    double newFactor = aFactor; // assume the factor is absolute...
    ZoomValue newValue = aZoomValue;

    Dimension viewSize = getSignalDiagram().getPreferredSize();

    // Ensure the zoom level is always bounded to the current view and the
    // number of samples...
    final Rectangle visibleViewSize = getVisibleViewSize();
    final SignalDiagramModel model = getModel();
    final double absLength = model.getAbsoluteLength();

    int newWidth;
    int newHeight = Math.max( viewSize.height, visibleViewSize.height );
    switch ( aZoomValue )
    {
      case IN:
      case OUT:
        // The given factor is relative...
        newFactor = aFactor * oldFactor;
        newValue = null;

        newWidth = Math.max( visibleViewSize.width, ( int )( viewSize.width * aFactor ) );
        break;

      case ALL:
        newFactor = visibleViewSize.width / absLength;
        newWidth = visibleViewSize.width;
        break;

      case MAXIMUM:
        newFactor = getMaxZoomLevel();
        newWidth = ( int )( absLength * newFactor );
        break;

      case DEFAULT:
      default:
        newFactor = getDefaultZoomLevel();
        newWidth = ( int )( absLength * newFactor );
        break;
    }

    // Make sure less samples do not cause empty bars on the screen...
    double oldViewWidth = Math.round( absLength * oldFactor );
    if ( oldViewWidth < visibleViewSize.width )
    {
      newFactor = visibleViewSize.width / absLength;
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

    if ( Double.compare( newFactor, minZoomLevel ) < 0.0 )
    {
      newFactor = minZoomLevel;
      newValue = ZoomValue.ALL;
    }
    else if ( Double.compare( newFactor, maxZoomLevel ) > 0.0 )
    {
      newFactor = maxZoomLevel;
      newValue = ZoomValue.MAXIMUM;
    }

    Dimension dimension = new Dimension( newWidth, newHeight );

    ZoomHolder newHolder = new ZoomHolder( newValue, newFactor );
    ZoomHolder oldHolder;

    do
    {
      oldHolder = this.zoomHolderRef.get();
    }
    while ( !this.zoomHolderRef.compareAndSet( oldHolder, newHolder ) );

    fireZoomEvent( createZoomEvent( aZoomValue, oldFactor, newFactor, centerPoint, dimension ) );
  }
}
