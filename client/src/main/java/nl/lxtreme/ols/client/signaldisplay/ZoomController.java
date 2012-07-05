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
import java.util.*;

import javax.swing.event.*;

import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Defines a zoom factor, with a ratio and some additional properties.
 */
public final class ZoomController
{
  // INNER TYPES

  /**
   * Denotes a zoom-event.
   */
  public static final class ZoomEvent
  {
    // VARIABLES

    private final double minZoomLevel;
    private final double defaultZoomLevel;
    private final double maxZoomLevel;
    private final double newFactor;
    private final double oldFactor;
    private final Point hotSpot;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ZoomEvent} instance.
     * 
     * @param aFactor
     * @param aMinZoomLevel
     * @param aMaxZoomLevel
     */
    public ZoomEvent( final double aNewFactor, final double aOldFactor, final double aMinZoomLevel,
        final double aDefaultLevel, final double aMaxZoomLevel, final Point aHotSpot )
    {
      this.newFactor = aNewFactor;
      this.oldFactor = aOldFactor;
      this.minZoomLevel = aMinZoomLevel;
      this.defaultZoomLevel = aDefaultLevel;
      this.maxZoomLevel = aMaxZoomLevel;
      this.hotSpot = aHotSpot == null ? null : new Point( aHotSpot );
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
      return this.newFactor;
    }

    /**
     * Returns the "hot spot" of this zoom event, e.g., where center location of
     * the zoom in/out action.
     * 
     * @return the "hot spot" on screen, can be <code>null</code>.
     */
    public Point getHotSpot()
    {
      return this.hotSpot;
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
     * Returns the current value of oldFactor.
     * 
     * @return the oldFactor
     */
    public double getOldFactor()
    {
      return this.oldFactor;
    }

    /**
     * Returns whether or not we're zooming to fit all.
     * 
     * @return <code>true</code> if zoom-all is enabled, <code>false</code>
     *         otherwise.
     */
    public boolean isZoomAll()
    {
      return Math.abs( this.newFactor - this.minZoomLevel ) < 1.0E-6;
    }

    /**
     * @return <code>true</code> if the default zoom level is selected,
     *         <code>false</code> otherwise.
     */
    public boolean isZoomMaximum()
    {
      return Math.abs( this.newFactor - this.maxZoomLevel ) < 1.0E-6;
    }

    /**
     * @return <code>true</code> if the default zoom level is selected,
     *         <code>false</code> otherwise.
     */
    public boolean isZoomOriginal()
    {
      return Math.abs( this.newFactor - this.defaultZoomLevel ) < 1.0E-6;
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

  // CONSTANTS

  /** The default/original zoom factor. */
  private static final double DEFAULT_ZOOM_FACTOR = 1.0;
  /** The zoom-ratio to use when zooming in (or out, if you use the inverse). */
  private static final double DEFAULT_ZOOM_RATIO = 2.0;

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

    final SignalDiagramModel model = this.controller.getSignalDiagramModel();
    final Rectangle viewSize = getSignalDiagram().getVisibleViewSize();

    final int width = Math.abs( aPoint2.x - aPoint1.x );
    final Long triggerPos = model.getTriggerPosition();

    ZoomEvent event;
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

      event = createZoomEvent( oldFactor, newFactor, hs );
    }

    fireZoomEvent( event );

    return true;
  }

  /**
   * Creates a new ZoomEvent instance, based on the current situation.
   * 
   * @return a new {@link ZoomEvent} instance, never <code>null</code>.
   */
  private ZoomEvent createZoomEvent( final double aOldFactor, final double aNewFactor, final Point aHotSpot )
  {
    return new ZoomEvent( aNewFactor, aOldFactor, getMinZoomLevel(), getDefaultZoomLevel(), getMaxZoomLevel(), aHotSpot );
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
  private void zoom( final ZoomValue aZoomValue, final double aFactor, final Point aHotSpot )
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

    if ( ( aZoomValue == ZoomValue.IN ) || ( aZoomValue == ZoomValue.OUT ) )
    {
      // Idea based on <http://stackoverflow.com/questions/115103>
      SignalDiagramComponent signalDiagram = getSignalDiagram();

      // Take the visibleRect of the signal diagram, as it tells us where we're
      // located in the scrollpane; this information we need to allow
      // dead-center zooming...
      double mx = signalDiagram.getVisibleRect().getCenterX();
      double my = 0;

      if ( aHotSpot != null )
      {
        mx = aHotSpot.x;
      }

      // Calculate the timestamp from the center position of the visible view
      // rectangle; after which we lookup the exact timestamp that is at that
      // position. If found, we'll use that timestamp to recalculate the new
      // center position in the new zoom factor...
      final long timestamp = ( long )( mx / oldFactor );
      final int tsIdx = model.getTimestampIndex( timestamp );

      mx = Math.floor( model.getTimestamps()[tsIdx] * oldFactor );

      // Take the location of the signal diagram component, as it is the only
      // one that is shifted in location by its (parent) scrollpane...
      final Point location = signalDiagram.getLocation();

      // Recalculate the new screen position of the visible view rectangle...
      int newX = ( int )( location.getX() - ( ( mx * aFactor ) - mx ) );
      int newY = ( int )( location.getY() - ( ( my * aFactor ) - my ) );

      signalDiagram.setLocation( newX, newY );
    }

    final ZoomEvent event;
    synchronized ( this.LOCK )
    {
      this.factor = newFactor;
      this.value = newValue;

      event = new ZoomEvent( newFactor, oldFactor, minZoomLevel, defaultZoomLevel, maxZoomLevel, aHotSpot );
    }

    fireZoomEvent( event );
  }
}
