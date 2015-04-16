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
package nl.lxtreme.ols.client.signaldisplay.view;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.action.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides an abstract mouse handler for the {@link SignalDiagramComponent}
 * views, such as {@link SignalView} and {@link TimeLineView}.
 */
abstract class AbstractMouseHandler extends MouseAdapter
{
  // CONSTANTS

  /**
   * Defines the area around each cursor in which the mouse cursor should be in
   * before the cursor can be moved.
   */
  private static final int CURSOR_SENSITIVITY_AREA = 4;

  // VARIABLES

  protected final SignalDiagramController controller;
  protected final PopupFactory popupHelper;

  private volatile int movingCursor;
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
    getViewComponent().requestFocusInWindow();

    if ( aEvent.getClickCount() == 2 )
    {
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      if ( getModel().isCursorMode() )
      {
        final Cursor hoveredCursor = findCursor( point );
        if ( hoveredCursor != null )
        {
          editCursorProperties( hoveredCursor );
          // Consume the event to stop further processing...
          aEvent.consume();
        }
      }

      // #132: Double clicking can mean a regular zoom event...
      if ( !aEvent.isConsumed() )
      {
        int rotation = ( aEvent.isAltDown() || aEvent.isShiftDown() ) ? 1 : -1;

        this.controller.getZoomController().zoom( rotation, point );
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

    if ( getModel().isCursorMode() && ( this.movingCursor >= 0 ) )
    {
      this.controller.moveCursor( this.movingCursor, getCursorDropPoint( point ) );

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
        Cursor hoveredCursor = findCursor( point );
        if ( hoveredCursor != null )
        {
          this.movingCursor = hoveredCursor.getIndex();
          setMouseCursor( aEvent, SignalView.CURSOR_MOVE_CURSOR );
        }
        else
        {
          this.movingCursor = -1;
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

    if ( !isCursorHover( event ) )
    {
      setMouseCursor( event, null );
    }

    if ( !handlePopupTrigger( point, aEvent ) )
    {
      this.movingCursor = -1;
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
   * Finds the cursor under the given point.
   *
   * @param aPoint
   *          the coordinate of the potential cursor, cannot be
   *          <code>null</code>.
   * @return the cursor index, or -1 if not found.
   */
  protected final Cursor findCursor( final Point aPoint )
  {
    final SignalDiagramModel model = getModel();

    final long refIdx = model.locationToTimestamp( aPoint );
    final double snapArea = CURSOR_SENSITIVITY_AREA / model.getZoomFactor();

    for ( Cursor cursor : model.getDefinedCursors() )
    {
      if ( cursor.inArea( refIdx, snapArea ) )
      {
        return cursor;
      }
    }

    return null;
  }

  /**
   * @return the {@link SignalDiagramModel}, never <code>null</code>.
   */
  protected final SignalDiagramModel getModel()
  {
    return this.controller.getViewModel();
  }

  /**
   * @return the view component, never <code>null</code>.
   */
  protected final JComponent getViewComponent()
  {
    return this.controller.getViewComponent();
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
  private JPopupMenu createCursorPopup( final Point aPoint, final Point aLocationOnScreen )
  {
    return this.popupHelper.createCursorPopup( findCursor( aPoint ), aPoint, aLocationOnScreen );
  }

  /**
   * Shows the "edit properties" dialog for the given cursor.
   *
   * @param aCursor
   *          the cursor to edit the properties for, cannot be <code>null</code>
   *          .
   */
  private void editCursorProperties( final Cursor aCursor )
  {
    ActionEvent stubEvent = new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" );
    new EditCursorPropertiesAction( this.controller, aCursor ).actionPerformed( stubEvent );
  }

  /**
   * Calculates the drop point for the cursor under the given coordinate.
   *
   * @param aCoordinate
   *          the coordinate to return the channel drop point for, cannot be
   *          <code>null</code>.
   * @return a drop point, never <code>null</code>.
   */
  private Point getCursorDropPoint( final Point aCoordinate )
  {
    Point dropPoint = new Point( aCoordinate );

    if ( getModel().isSnapCursorMode() )
    {
      final MeasurementInfo signalHover = getModel().getSignalHover( aCoordinate );
      if ( ( signalHover != null ) && !signalHover.isEmpty() )
      {
        dropPoint.x = signalHover.getMidSamplePos().intValue();
      }
    }
    dropPoint.y = 0;

    return dropPoint;
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
    final boolean popupTrigger = isCursorPopupTrigger( aEvent );
    if ( popupTrigger )
    {
      JPopupMenu contextMenu = createCursorPopup( aPoint, aEvent.getLocationOnScreen() );
      if ( contextMenu != null )
      {
        contextMenu.show( aEvent.getComponent(), aEvent.getX(), aEvent.getY() );
        // Mark the event as consumed...
        aEvent.consume();
      }
    }
    return popupTrigger;
  }

  /**
   * @param aEvent
   *          the event to test, may be <code>null</code>.
   * @return
   */
  private boolean isCursorHover( final MouseEvent aEvent )
  {
    if ( !isCursorPopupTrigger( aEvent ) )
    {
      return false;
    }

    final Point point = aEvent.getPoint();
    return ( findCursor( point ) != null );
  }

  /**
   * Returns whether or not the 'edit cursor' popup is to be shown.
   *
   * @param aEvent
   *          the event to test, may be <code>null</code>.
   * @return <code>true</code> if the 'edit cursor' popup is to be shown,
   *         <code>false</code> otherwise.
   */
  private boolean isCursorPopupTrigger( final MouseEvent aEvent )
  {
    return !aEvent.isConsumed() && aEvent.isPopupTrigger();
  }
}
