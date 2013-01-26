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
package nl.lxtreme.ols.client.ui.signaldisplay.view;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.marker.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.glasspane.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.signals.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.timeline.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an abstract mouse handler for the {@link SignalDiagramComponent}
 * views, such as {@link SignalView} and {@link TimeLineView}.
 */
public abstract class AbstractMouseHandler extends MouseAdapter
{
  // CONSTANTS

  /**
   * Defines the area around each cursor in which the mouse cursor should be in
   * before the cursor can be moved.
   */
  private static final int CURSOR_SENSITIVITY_AREA = 4;

  protected static final java.awt.Cursor CURSOR_MEASURE = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.CROSSHAIR_CURSOR );
  protected static final java.awt.Cursor CURSOR_MOVE_CURSOR = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.HAND_CURSOR );

  // VARIABLES

  protected final SignalDiagramController controller;
  protected final PopupFactory popupHelper;

  private volatile Marker movingMarker;
  private volatile Point lastClickPosition = null;

  // CONSTRUCTORS

  /**
   * Creates a new SignalDiagramComponent.MouseHandler instance.
   */
  public AbstractMouseHandler( final SignalDiagramController aController )
  {
    this.controller = aController;
    this.popupHelper = new PopupFactory( aController );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void mouseClicked( final MouseEvent aEvent )
  {
    // Ensure the focus is moved to the main signal diagram component...
    getSignalDiagram().requestFocusInWindow();

    if ( aEvent.getClickCount() == 2 )
    {
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      if ( getModel().isCursorMode() )
      {
        final Marker hoveredMarker = findMarker( point );
        if ( hoveredMarker != null )
        {
          editMarkerProperties( hoveredMarker );
          // Consume the event to stop further processing...
          aEvent.consume();
        }
      }

      // #132: Double clicking can mean a regular zoom event...
      if ( !aEvent.isConsumed() )
      {
        if ( aEvent.isAltDown() || aEvent.isShiftDown() )
        {
          // Zoom out...
          this.controller.getZoomController().zoomOut( point );
        }
        else
        {
          // Zoom in...
          this.controller.getZoomController().zoomIn( point );
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mouseDragged( final MouseEvent aEvent )
  {
    final MouseEvent event = convertEvent( aEvent );
    final Point point = event.getPoint();

    // Update the selected channel while dragging...
    this.controller.setSelectedChannel( point );

    if ( getModel().isCursorMode() && ( this.movingMarker != null ) && this.movingMarker.isMoveable() )
    {
      final Long timestamp = getMarkerDropPoint( point );
      if ( timestamp != null )
      {
        this.movingMarker.setTimestamp( timestamp.longValue() );
      }

      aEvent.consume();
    }
    else
    {
      if ( ( this.lastClickPosition == null ) && ( ( aEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK ) != 0 ) )
      {
        this.lastClickPosition = new Point( point );
      }

      final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, ( Component )aEvent.getSource() );
      if ( ( scrollPane != null ) && ( this.lastClickPosition != null ) )
      {
        final JViewport viewPort = scrollPane.getViewport();
        final Component signalView = viewPort.getView();

        boolean horizontalOnly = ( aEvent.getModifiersEx() & InputEvent.ALT_DOWN_MASK ) != 0;
        boolean verticalOnly = horizontalOnly && ( ( aEvent.getModifiersEx() & InputEvent.SHIFT_DOWN_MASK ) != 0 );

        int dx = aEvent.getX() - this.lastClickPosition.x;
        int dy = aEvent.getY() - this.lastClickPosition.y;

        Point scrollPosition = viewPort.getViewPosition();
        int newX = scrollPosition.x;
        if ( !verticalOnly )
        {
          newX -= dx;
        }
        int newY = scrollPosition.y;
        if ( verticalOnly || !horizontalOnly )
        {
          newY -= dy;
        }

        int diagramWidth = signalView.getWidth();
        int viewportWidth = viewPort.getWidth();
        int maxX = diagramWidth - viewportWidth - 1;
        scrollPosition.x = Math.max( 0, Math.min( maxX, newX ) );

        int diagramHeight = signalView.getHeight();
        int viewportHeight = viewPort.getHeight();
        int maxY = diagramHeight - viewportHeight;
        scrollPosition.y = Math.max( 0, Math.min( maxY, newY ) );

        viewPort.setViewPosition( scrollPosition );
      }

      // Use UNCONVERTED/ORIGINAL mouse event!
      handleZoomRegion( aEvent, this.lastClickPosition );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mousePressed( final MouseEvent aEvent )
  {
    final MouseEvent event = convertEvent( aEvent );
    final Point point = event.getPoint();

    if ( !handlePopupTrigger( point, aEvent ) )
    {
      if ( getModel().isCursorMode() )
      {
        Marker marker = findMarker( point );
        if ( ( marker != null ) && marker.isMoveable() )
        {
          this.movingMarker = marker;
          setMouseCursor( aEvent, CURSOR_MOVE_CURSOR );
        }
        else
        {
          this.movingMarker = null;
        }
      }

      if ( ( aEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK ) != 0 )
      {
        this.lastClickPosition = point;
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mouseReleased( final MouseEvent aEvent )
  {
    final MouseEvent event = convertEvent( aEvent );
    final Point point = event.getPoint();

    if ( !isMarkerHover( event ) )
    {
      setMouseCursor( event, null );
    }

    if ( !handlePopupTrigger( point, aEvent ) )
    {
      this.movingMarker = null;
    }

    if ( ( aEvent.getModifiersEx() & InputEvent.BUTTON1_MASK ) != 0 )
    {
      // Use UNCONVERTED/ORIGINAL mouse event!
      handleZoomRegion( aEvent, this.lastClickPosition );

      this.lastClickPosition = null;
    }
  }

  /**
   * @param aEvent
   * @return
   */
  protected final MouseEvent convertEvent( final MouseEvent aEvent )
  {
    JComponent view = SwingComponentUtils.getDeepestComponentAt( aEvent );
    return SwingUtilities.convertMouseEvent( aEvent.getComponent(), aEvent, view );
  }

  /**
   * Finds the marker under the given point.
   * 
   * @param aPoint
   *          the coordinate of the potential marker, cannot be
   *          <code>null</code>.
   * @return the hovered marker, or <code>null</code> if not found.
   */
  protected final Marker findMarker( final Point aPoint )
  {
    final SignalDiagramModel model = getModel();

    final long refIdx = model.locationToTimestamp( aPoint );
    final double snapArea = CURSOR_SENSITIVITY_AREA / model.getZoomFactor();

    for ( Marker marker : model.getDefinedMarkers() )
    {
      if ( marker.inArea( refIdx, snapArea ) )
      {
        return marker;
      }
    }

    return null;
  }

  /**
   * @return the {@link SignalDiagramModel}, never <code>null</code>.
   */
  protected final SignalDiagramModel getModel()
  {
    return this.controller.getSignalDiagramModel();
  }

  /**
   * @return the {@link SignalDiagramComponent}, never <code>null</code>.
   */
  protected final SignalDiagramComponent getSignalDiagram()
  {
    return this.controller.getSignalDiagram();
  }

  /**
   * @param aEvent
   * @param aStartPoint
   */
  protected void handleZoomRegion( final MouseEvent aEvent, final Point aStartPoint )
  {
    // For now, disabled by default as it isn't 100% working yet...
    if ( Boolean.FALSE.equals( Boolean.valueOf( System.getProperty( "zoomregionenabled", "false" ) ) ) )
    {
      return;
    }

    final JComponent source = ( JComponent )aEvent.getComponent();
    final boolean dragging = ( aEvent.getID() == MouseEvent.MOUSE_DRAGGED );

    final GhostGlassPane glassPane = ( GhostGlassPane )SwingUtilities.getRootPane( source ).getGlassPane();

    Rectangle viewRect;
    final JScrollPane scrollPane = SwingComponentUtils.getAncestorOfClass( JScrollPane.class, source );
    if ( scrollPane != null )
    {
      final JViewport viewport = scrollPane.getViewport();
      viewRect = SwingUtilities.convertRectangle( viewport, viewport.getVisibleRect(), glassPane );
    }
    else
    {
      viewRect = SwingUtilities.convertRectangle( source, source.getVisibleRect(), glassPane );
    }

    final Point start = SwingUtilities.convertPoint( source, aStartPoint, glassPane );
    final Point current = SwingUtilities.convertPoint( source, aEvent.getPoint(), glassPane );

    if ( dragging )
    {
      if ( !glassPane.isVisible() )
      {
        glassPane.setVisible( true );
        glassPane.setRenderer( new RubberBandRenderer(), start, current, viewRect );
      }
      else
      {
        glassPane.updateRenderer( start, current, viewRect );
      }

      glassPane.repaintPartially();
    }
    else
    /* if ( !dragging ) */
    {
      // Fire off a signal to the zoom controller to do its job...
      this.controller.getZoomController().zoomRegion( aStartPoint, aEvent.getPoint() );

      glassPane.setVisible( false );
    }
  }

  /**
   * Sets the current mouse cursor.
   * 
   * @param aMouseCursor
   *          a mouse cursor, can be <code>null</code> to use the default
   *          cursor.
   */
  protected final void setMouseCursor( final MouseEvent aEvent, final java.awt.Cursor aMouseCursor )
  {
    aEvent.getComponent().setCursor( aMouseCursor );
  }

  /**
   * Creates the context-sensitive popup menu for cursors.
   * 
   * @param aPoint
   *          the current mouse location to show the cursor, cannot be
   *          <code>null</code>;
   * @param aLocationOnScreen
   *          the location on screen, cannot be <code>null</code>.
   * @return a popup menu, never <code>null</code>.
   */
  private JPopupMenu createMarkerPopup( final Point aPoint, final Point aLocationOnScreen )
  {
    return this.popupHelper.createCursorPopup( findMarker( aPoint ), aPoint, aLocationOnScreen );
  }

  /**
   * Shows the "edit properties" dialog for the given marker.
   * 
   * @param aMarker
   *          the marker to edit the properties for, cannot be <code>null</code>
   *          .
   */
  private void editMarkerProperties( final Marker aMarker )
  {
    ActionEvent stubEvent = new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" );
    new EditMarkerPropertiesAction( aMarker ).actionPerformed( stubEvent );
  }

  /**
   * Calculates the drop point for the marker for the given coordinate.
   * 
   * @param aCoordinate
   *          the coordinate to return the marker drop point for, cannot be
   *          <code>null</code>.
   * @return a drop point, never <code>null</code>.
   */
  private Long getMarkerDropPoint( final Point aCoordinate )
  {
    int result = aCoordinate.x;

    if ( getModel().isSnapCursorMode() )
    {
      final MeasurementInfo signalHover = getModel().getSignalHover( aCoordinate );
      if ( ( signalHover != null ) && !signalHover.isEmpty() )
      {
        result = signalHover.getMidSamplePos().intValue();
      }
    }

    long markerLocation = getModel().locationToTimestamp( new Point( result, 0 ) ); // XXX
    if ( markerLocation < 0L )
    {
      return null;
    }

    return Long.valueOf( markerLocation );
  }

  /**
   * Determines whether or not the given mouse event is actually a popup
   * trigger.
   * 
   * @param aPoint
   *          the <em>corrected</em> mouse position, where the popup is to be
   *          shown, cannot be <code>null</code>;
   * @param aEvent
   *          the mouse event that could be a popup trigger, cannot be
   *          <code>null</code>.
   */
  private boolean handlePopupTrigger( final Point aPoint, final MouseEvent aEvent )
  {
    final boolean popupTrigger = isMarkerPopupTrigger( aEvent );
    if ( popupTrigger )
    {
      JPopupMenu contextMenu = createMarkerPopup( aPoint, aEvent.getLocationOnScreen() );
      if ( contextMenu != null )
      {
        // Mark the event as consumed...
        aEvent.consume();

        contextMenu.show( aEvent.getComponent(), aEvent.getX(), aEvent.getY() );
      }
    }
    return popupTrigger;
  }

  /**
   * @param aEvent
   *          the event to test, may be <code>null</code>.
   * @return
   */
  private boolean isMarkerHover( final MouseEvent aEvent )
  {
    if ( !isMarkerPopupTrigger( aEvent ) )
    {
      return false;
    }

    final Point point = aEvent.getPoint();
    return ( findMarker( point ) != null );
  }

  /**
   * Returns whether or not the 'edit cursor' popup is to be shown.
   * 
   * @param aEvent
   *          the event to test, may be <code>null</code>.
   * @return <code>true</code> if the 'edit cursor' popup is to be shown,
   *         <code>false</code> otherwise.
   */
  private boolean isMarkerPopupTrigger( final MouseEvent aEvent )
  {
    return !aEvent.isConsumed() && aEvent.isPopupTrigger();
  }
}
