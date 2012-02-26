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

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.action.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElement.*;
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
    // VARIABLES

    private final SignalDiagramController controller;

    private volatile int movingCursor;

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
      final Point point = aEvent.getPoint();

      if ( isEdgeWarpTrigger( aEvent, point ) )
      {
        final MeasurementInfo signalHover = getModel().getSignalHover( point );
        if ( ( signalHover != null ) && !signalHover.isEmpty() )
        {
          final long timestamp = signalHover.getEndTimestamp().longValue();

          this.controller.scrollToTimestamp( timestamp );
        }

        aEvent.consume();
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

      if ( !isCursorHover( aEvent ) && !isEdgeWarpTrigger( aEvent, point ) )
      {
        setMouseCursor( aEvent, null );
      }

      if ( !handlePopupTrigger( point, aEvent ) )
      {
        this.movingCursor = -1;
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
      return getModel().findCursor( aPoint );
    }

    /**
     * @return the {@link SignalDiagramModel}, never <code>null</code>.
     */
    protected final SignalDiagramModel getModel()
    {
      return this.controller.getSignalDiagramModel();
    }

    /**
     * Returns whether or not the given mouse event denotes a "edge warp"
     * trigger event.
     * 
     * @param aEvent
     *          the mouse event to test, cannot be <code>null</code>;
     * @param aPoint
     *          the (converted) mouse location, cannot be <code>null</code>.
     * @return <code>true</code> if the given mouse event represents a
     *         "edge warp" trigger event, <code>false</code> otherwise.
     */
    protected final boolean isEdgeWarpTrigger( final MouseEvent aEvent, final Point aPoint )
    {
      if ( ( aPoint == null ) || aEvent.isConsumed() )
      {
        return false;
      }
      if ( !this.controller.getSignalDiagramModel().hasData() || !isTimestampWarpModifier( aEvent ) )
      {
        return false;
      }

      return this.controller.getSignalHoverType( aPoint ) == SignalElementType.DIGITAL_SIGNAL;
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

      Cursor cursor = findCursor( aPoint );
      if ( cursor != null )
      {
        // Hovering above existing cursor, show remove menu...
        contextMenu.add( new EditCursorLabelAction( this.controller, cursor ) );
        contextMenu.addSeparator();

        contextMenu.add( new DeleteCursorAction( this.controller, cursor ) );
        contextMenu.add( new DeleteAllCursorsAction( this.controller ) );
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
        // when an action is selected, we *no* longer know where the point was
        // where the user clicked. Therefore, we need to store it separately
        // for later use...
        contextMenu.putClientProperty( SetCursorAction.KEY, getCursorDropPoint( aPoint ) );
      }

      return contextMenu;
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
      return !aEvent.isConsumed() && aEvent.isPopupTrigger() && getModel().isCursorMode();
    }
  }

  /**
   * 
   */
  private static final class MouseHandler extends BasicMouseHandler
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
        if ( isEdgeWarpTrigger( aEvent, point ) )
        {
          setMouseCursor( aEvent, CURSOR_MOVE_TIMESTAMP );
          aEvent.consume();
        }
        else
        {
          setMouseCursor( aEvent, null );
        }
      }
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
   * Returns whether or not the given input event can be interpreted as a 'edge
   * warp' trigger event.
   * 
   * @param aEvent
   *          the input event to test, cannot be <code>null</code>.
   * @return <code>true</code> if the given input event is a 'edge warp' trigger
   *         event, <code>false</code> otherwise.
   */
  public static boolean isTimestampWarpModifier( final InputEvent aEvent )
  {
    boolean modifierDown;
    if ( isMacOS() )
    {
      // Is the CMD key...
      modifierDown = aEvent.isMetaDown();
    }
    else
    {
      modifierDown = aEvent.isControlDown();
    }
    return modifierDown;
  }

  /**
   * Returns whether the current host's operating system is Mac OS X.
   * 
   * @return <code>true</code> if running on Mac OS X, <code>false</code>
   *         otherwise.
   */
  private static boolean isMacOS()
  {
    final String osName = System.getProperty( "os.name" );
    return ( "Mac OS X".equalsIgnoreCase( osName ) || "Darwin".equalsIgnoreCase( osName ) );
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
    // addMouseWheelListener( this.mouseHandler );

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
      setToolTipText( aEvent.toHtmlString() );
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
    removeMouseWheelListener( this.mouseHandler );

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
}
