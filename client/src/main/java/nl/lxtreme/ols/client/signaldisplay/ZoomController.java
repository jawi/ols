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

    private final boolean zoomAll;
    private final double minZoomLevel;
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
    public ZoomEvent( final boolean isZoomAll, final double aNewFactor, final double aOldFactor,
        final double aMinZoomLevel, final double aMaxZoomLevel, final Point aHotSpot )
    {
      this.zoomAll = isZoomAll;
      this.newFactor = aNewFactor;
      this.oldFactor = aOldFactor;
      this.minZoomLevel = aMinZoomLevel;
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
     * @return <code>true</code> if there's a change in the zoom factor,
     *         <code>false</code> if the zoom factor remains the same.
     */
    public boolean isFactorChange()
    {
      return Double.compare( this.oldFactor, this.newFactor ) != 0;
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

  // CONSTANTS

  /** Tells the zoom controller to zoom in with a factor of 2. */
  public static int ZOOM_IN = -1;
  /** Tells the zoom controller to zoom out with a factor of 2. */
  public static int ZOOM_OUT = 1;

  /** The default/original zoom factor. */
  private static final double DEFAULT_ZOOM_FACTOR = 1.0;
  /** The zoom-ratio to use when zooming in (or out, if you use the inverse). */
  private static final double DEFAULT_ZOOM_RATIO = 2.0;

  // VARIABLES

  private final SignalDiagramController controller;
  private final EventListenerList eventListeners;

  private final Object LOCK = new Object();

  private boolean zoomAll = false;
  private double factor = Double.NaN;

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
      return this.zoomAll;
    }
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
    final ZoomEvent event;
    synchronized ( this.LOCK )
    {
      // Initially, factor = NaN; so we do this trick to make sure there's a
      // 'factor change' for the initial zoom event as well...
      final double oldFactor = this.factor;
      final double newFactor = getFactor();

      event = createZoomEvent( oldFactor, newFactor, null );
    }
    fireZoomEvent( event );
  }

  /**
   * Zooms in with a factor 2.
   */
  public void zoom( final int aDirection )
  {
    zoom( aDirection, null );
  }

  /**
   * Zooms in with a factor 2.
   */
  public void zoom( final int aDirection, final Point aHotSpot )
  {
    final ZoomEvent event;
    synchronized ( this.LOCK )
    {
      double oldFactor = getFactor();
      double newFactor = setFactor( calculateNewFactor( aDirection == ZOOM_IN ? DEFAULT_ZOOM_RATIO
          : 1.0 / DEFAULT_ZOOM_RATIO ) );

      this.zoomAll = false;

      event = createZoomEvent( oldFactor, newFactor, aHotSpot );
    }
    fireZoomEvent( event );
  }

  /**
   * Zooms to make all data visible in one screen.
   */
  public void zoomAll()
  {
    final ZoomEvent event;
    synchronized ( this.LOCK )
    {
      double oldFactor = getFactor();
      double newFactor = setFactor( getMinZoomLevel() );

      this.zoomAll = true;

      event = createZoomEvent( oldFactor, newFactor, null );
    }
    fireZoomEvent( event );
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public void zoomOriginal()
  {
    final ZoomEvent event;
    synchronized ( this.LOCK )
    {
      double oldFactor = getFactor();
      double newFactor = setFactor( DEFAULT_ZOOM_FACTOR );

      this.zoomAll = false;

      event = createZoomEvent( oldFactor, newFactor, null );
    }
    fireZoomEvent( event );
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
    final Rectangle viewSize = this.controller.getSignalDiagram().getVisibleViewSize();

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

      double newFactor = setFactor( ( viewSize.width / ( double )width ) );

      this.zoomAll = false;

      event = createZoomEvent( oldFactor, newFactor, hs );
    }

    fireZoomEvent( event );

    return true;
  }

  /**
   * Calculates the new zoom-factor, based on a given zoom ratio.
   * 
   * @param aZoomRatio
   *          the zoom-ratio to use, != 0.0.
   * @return a new zoom factor, bounded to the minimum and maximum zoom levels.
   */
  private double calculateNewFactor( final double aZoomRatio )
  {
    return Math.max( getMinZoomLevel(), Math.min( getMaxZoomLevel(), aZoomRatio * getFactor() ) );
  }

  /**
   * Creates a new ZoomEvent instance, based on the current situation.
   * 
   * @return a new {@link ZoomEvent} instance, never <code>null</code>.
   */
  private ZoomEvent createZoomEvent( final double aOldFactor, final double aNewFactor, final Point aHotSpot )
  {
    return new ZoomEvent( this.zoomAll, aNewFactor, aOldFactor, getMinZoomLevel(), getMaxZoomLevel(), aHotSpot );
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

    Rectangle viewSize = this.controller.getSignalDiagram().getVisibleViewSize();
    final double length = model.getAbsoluteLength();

    return viewSize.getWidth() / length;
  }

  /**
   * @return the signal diagram model, never <code>null</code>.
   */
  private SignalDiagramModel getModel()
  {
    return this.controller.getSignalDiagramModel();
  }

  /**
   * Sets the new zoom factor to the one given.
   * <p>
   * SHOULD ONLY BE CALLED FROM A SYNCHRONIZED BLOCK!
   * </p>
   * 
   * @param aFactor
   *          the new zoom factor to set.
   */
  private double setFactor( final double aFactor )
  {
    return this.factor = aFactor;
  }
}
