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
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.atomic.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.views.*;


/**
 * Defines a zoom factor, with a ratio and some additional properties.
 */
final class ZoomController
{
  // INNER TYPES

  /**
   * Denotes the value with which zooming in/out should happen.
   */
  public static enum ZoomAction
  {
    // CONSTANTS

    /**
     * Keeps zoom-level as-is, useful for redrawing the views after changing
     * their dimensions.
     */
    RESTORE,
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

  /**
   * Small container for keeping the zoom state.
   */
  private static class ZoomStateHolder
  {
    // VARIABLES

    final double factor;
    final ZoomAction lastAction;

    // CONSTRUCTORS

    public ZoomStateHolder( ZoomAction aAction, double aFactor )
    {
      this.lastAction = aAction;
      this.factor = aFactor;
    }
  }

  // CONSTANTS

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

  /** The default/original zoom factor. */
  private static final double DEFAULT_ZOOM_FACTOR = 1.0;
  /** The zoom-ratio to use when zooming in (or out, if you use the inverse). */
  private static final double DEFAULT_ZOOM_RATIO = 2.0;

  // VARIABLES

  private final ViewController controller;

  private final AtomicReference<ZoomStateHolder> zoomHolderRef;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ZoomController} instance.
   * 
   * @param aController
   *          the view controller to use, cannot be <code>null</code>.
   */
  public ZoomController( ViewController aController )
  {
    this.controller = aController;

    this.zoomHolderRef = new AtomicReference<ZoomStateHolder>();
  }

  // METHODS

  /**
   * Returns the current value of factor.
   * 
   * @return the factor
   */
  public double getFactor()
  {
    ZoomStateHolder zh = this.zoomHolderRef.get();
    if ( zh == null || Double.isNaN( zh.factor ) )
    {
      return DEFAULT_ZOOM_FACTOR;
    }
    return zh.factor;
  }

  /**
   * Returns whether or not we're zooming to fit all.
   * 
   * @return <code>true</code> if zoom-all is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isZoomAll()
  {
    ZoomStateHolder zh = this.zoomHolderRef.get();
    if ( zh == null )
    {
      return false;
    }
    ZoomAction value = zh.lastAction;

    return ( value == ZoomAction.ALL );
  }

  /**
   * @return <code>true</code> if the default zoom level is selected,
   *         <code>false</code> otherwise.
   */
  public boolean isZoomDefault()
  {
    ZoomStateHolder zh = this.zoomHolderRef.get();
    if ( zh == null )
    {
      return false;
    }
    ZoomAction value = zh.lastAction;

    return ( value == ZoomAction.DEFAULT );
  }

  /**
   * Restores the zoom-level to the current level, and notifies all listeners.
   */
  public void restoreZoomLevel()
  {
    ZoomAction action = null;
    double factor = DEFAULT_ZOOM_FACTOR;

    ZoomStateHolder zh = this.zoomHolderRef.get();
    if ( zh != null )
    {
      action = zh.lastAction;
      factor = zh.factor;
    }
    if ( action == null || ZoomAction.IN == action || ZoomAction.OUT == action )
    {
      action = ZoomAction.RESTORE;
    }

    performZoomAction( action, factor, null );
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
      performZoomAction( ZoomAction.OUT, ratio, aPoint );
    }
    else if ( aRotation < 0 )
    {
      double ratio = ( -aRotation * DEFAULT_ZOOM_RATIO );
      performZoomAction( ZoomAction.IN, ratio, aPoint );
    }
  }

  /**
   * Zooms to make the entire view visible.
   */
  public void zoomAll()
  {
    performZoomAction( ZoomAction.ALL, 0.0 /* not used */, null );
  }

  /**
   * Zooms in with a constant factor around the current view-center.
   */
  public void zoomIn()
  {
    performZoomAction( ZoomAction.IN, DEFAULT_ZOOM_RATIO, null );
  }

  /**
   * Zooms to make the most detailed view possible.
   */
  public void zoomMaximum()
  {
    performZoomAction( ZoomAction.MAXIMUM, 0.0 /* not used */, null );
  }

  /**
   * Zooms to the default zoom level.
   */
  public void zoomOriginal()
  {
    performZoomAction( ZoomAction.DEFAULT, 0.0 /* not used */, null );
  }

  /**
   * Zooms out with a constant factor around the current view-center.
   */
  public void zoomOut()
  {
    performZoomAction( ZoomAction.OUT, 1.0 / DEFAULT_ZOOM_RATIO, null );
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
   * Returns whether or not we can zoom in further.
   * 
   * @return <code>true</code> if we can zoom in, <code>false</code> if the
   *         maximum zoom level has been reached.
   */
  protected boolean canZoomIn()
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
  protected boolean canZoomOut()
  {
    final double minZoomLevel = getMinZoomLevel();
    final double zoomFactor = getFactor();
    return zoomFactor > minZoomLevel;
  }

  /**
   * Determines the new zoom state for the given action and factor.
   * 
   * @param aAction
   *          the zoom action to perform, cannot be <code>null</code>;
   * @param aFactor
   *          the relative zoom factor to apply.
   * @return the new zoom state, never <code>null</code>.
   */
  private ZoomStateHolder calculateNewZoomState( ZoomAction aAction, double aFactor )
  {
    final double oldFactor = getFactor();

    final double minZoomLevel = getMinZoomLevel();
    final double maxZoomLevel = getMaxZoomLevel();
    final double defaultZoomLevel = Math.max( minZoomLevel, DEFAULT_ZOOM_FACTOR );

    double newFactor;
    ZoomAction newValue = aAction;

    // Determine what to do...
    switch ( aAction )
    {
      case IN:
      case OUT:
        // The given factor is relative...
        newFactor = aFactor * oldFactor;
        break;

      case ALL:
        newFactor = minZoomLevel;
        break;

      case MAXIMUM:
        newFactor = maxZoomLevel;
        break;

      case DEFAULT:
        newFactor = defaultZoomLevel;
        break;

      case RESTORE:
      default:
        newFactor = oldFactor;
        break;
    }

    // Make sure the default zoom-level is selected when we're close enough to
    // its value...
    if ( Math.abs( newFactor - defaultZoomLevel ) < 1.0e-6 )
    {
      newFactor = defaultZoomLevel;
      newValue = ZoomAction.DEFAULT;
    }

    // Make sure we do not go beyond the minimum and maximum zoom levels...
    if ( newFactor < minZoomLevel )
    {
      newFactor = minZoomLevel;
      newValue = ZoomAction.ALL;
    }
    else if ( newFactor > maxZoomLevel )
    {
      newFactor = maxZoomLevel;
      newValue = ZoomAction.MAXIMUM;
    }

    return new ZoomStateHolder( newValue, newFactor );
  }

  /**
   * Calculates the visible rectangle of the view component given a zoom-action,
   * a relative zoom-factor, a center position and the new zoom state.
   * 
   * @return a rectangle denoting the visible view of the component, never
   *         <code>null</code>.
   */
  private Rectangle calculateVisibleViewRect( final ZoomStateHolder aZoomState, final Point aCenterPoint )
  {
    // Take the location of the signal diagram component, as it is the
    // only one that is shifted in location by its (parent) scrollpane...
    Point currentLocation = getWaveformView().getLocation();

    Rectangle currentVisibleRect = getWaveformView().getVisibleRect();

    int mx = ( aCenterPoint != null ) ? aCenterPoint.x : ( int )currentVisibleRect.getCenterX();

    Rectangle visibleRect = new Rectangle();

    // Calculate the relative factor we're using...
    double relFactor = aZoomState.factor / getFactor();

    // Calculate the new dimensions and the new location of the view...
    switch ( aZoomState.lastAction )
    {
      case IN:
      case OUT:
      {
        // Use the given (relative!) factor to calculate the new
        // width of the view!
        Dimension viewSize = getWaveformView().getPreferredSize();
        visibleRect.width = ( int )( viewSize.width * relFactor );
        visibleRect.height = currentVisibleRect.height;
        // Recalculate the new screen position of the visible view
        // rectangle; the X-coordinate shifts relative to the zoom
        // factor, while the Y-coordinate remains as-is...
        visibleRect.x = ( int )Math.round( currentLocation.x - ( mx * relFactor ) + mx );
        visibleRect.y = currentLocation.y;
        break;
      }
      case ALL:
      {
        // The new width of the view is always the same as the width of the view
        // size...
        Dimension outerViewSize = getOuterViewSize( getWaveformView(), true, true );
        visibleRect.width = outerViewSize.width;
        visibleRect.height = outerViewSize.height;
        // Since everything fits on screen, we can reset the view location to
        // its initial X-coordinate...
        visibleRect.x = 0;
        visibleRect.y = currentLocation.y;
        break;
      }
      case MAXIMUM:
      case DEFAULT:
      {
        // Recalculate the new width based on the max./default zoom level...
        visibleRect.width = ( int )( getModel().getAbsoluteLength() * aZoomState.factor );
        visibleRect.height = currentVisibleRect.height;
        // Recalculate the new screen position of the visible view
        // rectangle; the X-coordinate shifts relative to the zoom
        // factor, while the Y-coordinate remains as-is...
        visibleRect.x = ( int )Math.round( currentLocation.x - ( mx * relFactor ) + mx );
        visibleRect.y = currentLocation.y;
        break;
      }
      case RESTORE:
      default:
      {
        // Recalculate the new width based on the old zoom level...
        visibleRect.width = ( int )( getModel().getAbsoluteLength() * aZoomState.factor );
        visibleRect.height = currentVisibleRect.height;
        // Keep the location as-is...
        visibleRect.x = currentLocation.x;
        visibleRect.y = currentLocation.y;
        break;
      }
    }

    // Ensure the visible location stays in the calculated minimum and
    // maximum...
    int maxX = ( visibleRect.width - currentVisibleRect.width );
    if ( Math.abs( visibleRect.x ) > maxX )
    {
      visibleRect.x = -maxX;
    }
    // View locations appear to be always negative with respect to the (0,
    // 0)-coordinate...
    if ( visibleRect.x > 0 )
    {
      visibleRect.x = -visibleRect.x;
    }

    // Ensure the minimum height of the view is adhered...
    int minimumHeight = getModel().calculateScreenHeight();
    if ( visibleRect.height < minimumHeight )
    {
      visibleRect.height = minimumHeight;
    }
    // Try to suppress the vertical scrollbar, if possible...
    if ( visibleRect.width > currentVisibleRect.width )
    {
      JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, getWaveformView() );
      if ( scrollPane != null )
      {
        JScrollBar horizontalScrollBar = scrollPane.getHorizontalScrollBar();
        int sbHeight = horizontalScrollBar.getHeight();
        // When the width of the view is wider than we can currently view, the
        // horizontal scrollbar is not yet visible, and we have enough room
        // vertically, subtract the scrollbar height in order to suppress the
        // vertical scrollbar...
        if ( !horizontalScrollBar.isVisible() && ( visibleRect.height > ( minimumHeight + sbHeight ) )
            && ( visibleRect.height == currentVisibleRect.height ) )
        {
          visibleRect.height -= sbHeight;
        }
      }
    }

    return visibleRect;
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
   * @see #MAX_COMP_WIDTH
   */
  private double getMaxZoomLevel()
  {
    final double length = getModel().getAbsoluteLength();

    return MAX_COMP_WIDTH / length;
  }

  /**
   * Determines the minimum zoom level that we can causes all signals to be
   * displayed in the current width and height.
   * 
   * @return a minimum zoom level.
   */
  private double getMinZoomLevel()
  {
    final double width = getOuterViewSize( getWaveformView(), true, true ).width;
    final double length = getModel().getAbsoluteLength();
    final double min = 1.0 / MAX_COMP_WIDTH;

    return Math.max( min, width / length );
  }

  private WaveformModel getModel()
  {
    return ( WaveformModel )this.controller.getModel();
  }

  /**
   * @return the view size of the given component, including any width/height of
   *         visible scrollbars.
   */
  private Dimension getOuterViewSize( JComponent aComponent, boolean aIncludeVertScrollbar,
      boolean aIncludeHorzScrollbar )
  {
    Rectangle rect = aComponent.getVisibleRect();
    Dimension size = rect.getSize();

    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, aComponent );
    if ( scrollPane != null )
    {
      // Take care of the fact that scrollbars *can* be visible, which is not
      // what we want here. We want to have the outside boundaries, including
      // the scrollbars...
      JScrollBar scrollBar = scrollPane.getVerticalScrollBar();
      if ( aIncludeVertScrollbar && scrollBar.isVisible() )
      {
        size.width += scrollBar.getWidth();
      }
      scrollBar = scrollPane.getHorizontalScrollBar();
      if ( aIncludeHorzScrollbar && scrollBar.isVisible() )
      {
        size.height += scrollBar.getHeight();
      }

      final Insets insets = scrollPane.getViewport().getInsets();
      size.width -= ( insets.left + insets.right );
      size.height -= ( insets.top + insets.bottom );
    }

    return size;
  }

  private WaveformView getView()
  {
    return ( WaveformView )this.controller.getView();
  }

  private WaveformViewComponent getWaveformView()
  {
    WaveformView view = getView();
    return view.getViewComponent();
  }

  /**
   * Performs the zoom action as given using the given relative factor and
   * center point.
   * 
   * @param aAction
   * @param aFactor
   * @param aCenterPoint
   */
  private void performZoomAction( final ZoomAction aAction, final double aFactor, final Point aCenterPoint )
  {
    ZoomStateHolder newState = calculateNewZoomState( aAction, aFactor );
    Rectangle visibleRect = calculateVisibleViewRect( newState, aCenterPoint );

    ZoomStateHolder oldState;
    do
    {
      oldState = this.zoomHolderRef.get();
    }
    while ( !this.zoomHolderRef.compareAndSet( oldState, newState ) );

    // Update the zoom action's state...
    updateZoomActions();

    // Update the view size...
    updateViewSize( visibleRect );
  }

  /**
   * Updates the preferred width and location of the view.
   * 
   * @param aRectangle
   *          the new visible rectangle of the view component, cannot be
   *          <code>null</code>.
   */
  private void updateViewSize( Rectangle aRectangle )
  {
    WaveformView view = getView();
    view.updatePreferredSize( aRectangle.getSize(), aRectangle.getLocation() );
  }

  /**
   * Updates the state of all zoom-related actions, do this here instead of in
   * the individual zoom actions as we have all information present here, which
   * we otherwise have to expose to the actions.
   */
  private void updateZoomActions()
  {
    boolean zoomCapable = getView().canZoom();

    ActionManager actionManager = this.controller.getActionManager();
    Action zoomInAction = actionManager.getAction( ZoomInAction.ID );
    zoomInAction.setEnabled( zoomCapable && canZoomIn() );

    Action zoomOutAction = actionManager.getAction( ZoomOutAction.ID );
    zoomOutAction.setEnabled( zoomCapable && canZoomOut() );

    Action zoomAllAction = actionManager.getAction( ZoomAllAction.ID );
    zoomAllAction.setEnabled( zoomCapable && !isZoomAll() );

    Action zoomOriginalAction = actionManager.getAction( ZoomOriginalAction.ID );
    zoomOriginalAction.setEnabled( zoomCapable && !isZoomDefault() );
  }
}
