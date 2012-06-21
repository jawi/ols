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
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;
import java.awt.event.*;

import static nl.lxtreme.ols.util.swing.SwingComponentUtils.getAncestorOfClass;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.action.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.renderer.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a view for the signal data as individual channels.
 */
public class SignalView extends AbstractViewLayer implements IMeasurementListener, ICursorChangeListener
{
  // INNER TYPES

  /**
   * Mouse handler for the {@link SignalDiagramComponent}.
   */
  static class BasicMouseHandler extends MouseAdapter
  {
    // CONSTANTS

    /**
     * Defines the area around each cursor in which the mouse cursor should be
     * in before the cursor can be moved.
     */
    private static final int CURSOR_SENSITIVITY_AREA = 4;

    // VARIABLES

    protected final SignalDiagramController controller;

    private volatile int movingCursor;
    private volatile Point lastClickPosition = null;

    // CONSTRUCTORS

    /**
     * Creates a new SignalDiagramComponent.MouseHandler instance.
     */
    public BasicMouseHandler( final SignalDiagramController aController )
    {
      this.controller = aController;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseClicked( final MouseEvent aEvent )
    {
      if ( getModel().isCursorMode() && ( aEvent.getClickCount() == 2 ) )
      {
        final MouseEvent event = convertEvent( aEvent );

        final Cursor hoveredCursor = findCursor( event.getPoint() );
        if ( hoveredCursor != null )
        {
          editCursorProperties( hoveredCursor );
          // Consume the event to stop further processing...
          aEvent.consume();
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
          final Component signalView = this.controller.getSignalDiagram().getSignalView();

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
    public void mouseMoved( final MouseEvent aEvent )
    {
      final MouseEvent event = convertEvent( aEvent );
      final Point point = event.getPoint();

      final SignalDiagramModel model = getModel();
      if ( model.isCursorMode() )
      {
        final Cursor hoveredCursor = findCursor( point );
        if ( hoveredCursor != null )
        {
          setMouseCursor( aEvent, CURSOR_MOVE_CURSOR );
          aEvent.consume();
        }
        else
        {
          setMouseCursor( aEvent, null );
          aEvent.consume();
        }
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
            setMouseCursor( aEvent, CURSOR_MOVE_CURSOR );
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
      return this.controller.getSignalDiagramModel();
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
      final JPopupMenu contextMenu = new JPopupMenu();

      Action smartJumpLeftAction = this.controller.getActionManager().getAction( SmartJumpAction.getJumpLeftID() );
      Action smartJumpRightAction = this.controller.getActionManager().getAction( SmartJumpAction.getJumpRightID() );

      contextMenu.add( smartJumpLeftAction );
      contextMenu.add( smartJumpRightAction );
      contextMenu.addSeparator();

      // when an action is selected, we *no* longer know where the point was
      // where the user clicked. Therefore, we need to store it separately
      // for later use...
      contextMenu.putClientProperty( "mouseLocation", aPoint );

      Cursor cursor = findCursor( aPoint );
      if ( cursor != null )
      {
        // Hovering above existing cursor, show remove menu...
        contextMenu.add( new DeleteCursorAction( this.controller, cursor ) );
        contextMenu.add( new DeleteAllCursorsAction( this.controller ) );

        contextMenu.addSeparator();
        contextMenu.add( new EditCursorPropertiesAction( this.controller, cursor ) );
      }
      else
      {
        // Not hovering above existing cursor, show add menu...
        for ( int i = 0; i < Ols.MAX_CURSORS; i++ )
        {
          final SetCursorAction action = new SetCursorAction( this.controller, i );
          contextMenu.add( new JCheckBoxMenuItem( action ) );
        }
        contextMenu.addSeparator();
        contextMenu.add( new DeleteAllCursorsAction( this.controller ) );

        contextMenu.putClientProperty( SetCursorAction.KEY, getCursorDropPoint( aPoint ) );
      }

      return contextMenu;
    }

    /**
     * Shows the "edit properties" dialog for the given cursor.
     * 
     * @param aCursor
     *          the cursor to edit the properties for, cannot be
     *          <code>null</code>.
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

      if ( getModel().isSnapCursor() )
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
        contextMenu.show( aEvent.getComponent(), aEvent.getX(), aEvent.getY() );
        // Mark the event as consumed...
        aEvent.consume();
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

  /**
   * 
   */
  private final class MouseHandler extends BasicMouseHandler
  {
    // VARIABLES

    private volatile MeasurementInfo lastMeasurementInfo;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MouseHandler} instance.
     */
    public MouseHandler( final SignalDiagramController aController )
    {
      super( aController );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseMoved( final MouseEvent aEvent )
    {
      final MouseEvent event = convertEvent( aEvent );
      final Point point = event.getPoint();

      final SignalDiagramModel model = getModel();

      this.controller.setSelectedChannel( point );

      if ( model.isCursorMode() || model.isMeasurementMode() )
      {
        if ( model.isMeasurementMode() )
        {
          this.lastMeasurementInfo = model.getSignalHover( point );

          if ( this.lastMeasurementInfo != null )
          {
            setMouseCursor( aEvent, CURSOR_HOVER );

            model.fireMeasurementEvent( this.lastMeasurementInfo );
            aEvent.consume();
          }
        }

        if ( model.isCursorMode() )
        {
          final Cursor hoveredCursor = findCursor( point );
          if ( hoveredCursor != null )
          {
            setMouseCursor( aEvent, CURSOR_MOVE_CURSOR );
            aEvent.consume();
          }
        }
      }

      if ( !aEvent.isConsumed() )
      {
        setMouseCursor( aEvent, null );

        SignalElement element = findSignalElement( point );
        if ( ( element != null ) && element.isDigitalSignal() )
        {
          final AnnotationsHelper helper = new AnnotationsHelper( element );

          final long timestamp = getModel().locationToTimestamp( point );

          Annotation<?> annotation = helper.getAnnotation( timestamp );
          if ( annotation != null )
          {
            SignalView view = ( SignalView )aEvent.getSource();
            view.setToolTipText( annotation.getAnnotation().toString() );

            aEvent.consume();
          }
        }
      }
    }

    /**
     * Finds the channel under the given point.
     * 
     * @param aPoint
     *          the coordinate of the potential channel, cannot be
     *          <code>null</code>.
     * @return the channel index, or -1 if not found.
     */
    private SignalElement findSignalElement( final Point aPoint )
    {
      return this.controller.getSignalDiagramModel().findSignalElement( aPoint );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  static final java.awt.Cursor CURSOR_WAIT = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR );
  static final java.awt.Cursor CURSOR_HOVER = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.CROSSHAIR_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_CURSOR = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.HAND_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_TIMESTAMP = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.E_RESIZE_CURSOR );

  // VARIABLES

  private final SignalViewModel model;
  private final BasicMouseHandler mouseHandler;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   */
  private SignalView( final SignalDiagramController aController )
  {
    super( aController );

    this.model = new SignalViewModel( aController );

    this.mouseHandler = new MouseHandler( aController );

    updateUI();
  }

  // METHODS

  /**
   * @param aController
   * @return
   */
  public static SignalView create( final SignalDiagramController aController )
  {
    final SignalView signalView = new SignalView( aController );

    aController.addCursorChangeListener( signalView );
    aController.addMeasurementListener( signalView );

    return signalView;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    updateUI();

    addMouseListener( this.mouseHandler );
    addMouseMotionListener( this.mouseHandler );

    super.addNotify();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorAdded( final Cursor aCursor )
  {
    final int visibleHeight = getVisibleRect().height;

    final SignalViewModel model = getModel();

    int cursorPos = model.timestampToCoordinate( aCursor.getTimestamp() );
    repaint( new Rectangle( cursorPos - 1, 0, 2, visibleHeight ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorChanged( final String aPropertyName, final Cursor aOldCursor, final Cursor aNewCursor )
  {
    final int visibleHeight = getVisibleRect().height;

    final SignalViewModel model = getModel();

    int cursorPos = model.timestampToCoordinate( aOldCursor.getTimestamp() );
    repaint( 0, cursorPos - 1, 0, 2, visibleHeight );

    cursorPos = model.timestampToCoordinate( aNewCursor.getTimestamp() );
    repaint( 0, cursorPos - 1, 0, 2, visibleHeight );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorRemoved( final Cursor aOldCursor )
  {
    final int visibleHeight = getVisibleRect().height;

    final SignalViewModel model = getModel();

    int cursorPos = model.timestampToCoordinate( aOldCursor.getTimestamp() );
    repaint( 0, cursorPos - 1, 0, 2, visibleHeight );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorsInvisible()
  {
    repaint( 50L );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorsVisible()
  {
    repaint( 50L );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void disableMeasurementMode()
  {
    final SignalUI signalUI = ( SignalUI )this.ui;

    final Rectangle oldRect = signalUI.getMeasurementRect();
    if ( oldRect != null )
    {
      repaint( oldRect );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void enableMeasurementMode()
  {
    // Nothing special to do for this event...
  }

  /**
   * Returns the current value of model.
   * 
   * @return the model
   */
  public SignalViewModel getModel()
  {
    return this.model;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleMeasureEvent( final MeasurementInfo aEvent )
  {
    final SignalUI signalUI = ( SignalUI )this.ui;

    final Rectangle oldRect = signalUI.getMeasurementRect();

    signalUI.handleMeasureEvent( aEvent );

    final Rectangle newRect = signalUI.getMeasurementRect();

    if ( aEvent != null )
    {
      setToolTipText( ViewUtils.getToolTipText( getSignalDiagramModel(), aEvent.getReferenceTime() ) );
    }
    else
    {
      setToolTipText( null );
    }

    if ( oldRect != null )
    {
      repaint( oldRect );
    }
    if ( newRect != null )
    {
      repaint( newRect );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isListening()
  {
    return ( ( SignalUI )this.ui ).isListening();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeNotify()
  {
    removeMouseListener( this.mouseHandler );
    removeMouseMotionListener( this.mouseHandler );

    super.removeNotify();
  }

  /**
   * Overridden in order to set a custom UI, which not only paints this diagram,
   * but also can be used to manage the various settings, such as colors,
   * height, and so on.
   * 
   * @see javax.swing.JComponent#updateUI()
   */
  @Override
  public final void updateUI()
  {
    setUI( new SignalUI() );
  }

  /**
   * @return the current signal diagram model, never <code>null</code>.
   */
  private SignalDiagramModel getSignalDiagramModel()
  {
    return getController().getSignalDiagramModel();
  }
}
