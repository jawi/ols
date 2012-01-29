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
package nl.lxtreme.ols.client.signaldisplay;


import static nl.lxtreme.ols.util.DisplayUtils.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.logging.*;

import javax.swing.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.signaldisplay.action.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a signal diagram, where signals in the form of sample data is
 * represented by channels.
 */
public class SignalDiagramComponent extends JPanel implements Scrollable
{
  // INNER TYPES

  /**
   * Provides a transparent event listener to allow some of the functionality
   * (such as DnD and cursor dragging) of this component to be controlled with
   * the mouse and keyboard. It is implemented as an {@link AWTEventListener} to
   * make it "transparent" to the rest of the components. Without this, the
   * events would be consumed without getting propagated to the actual
   * scrollpane.
   */
  final class TransparentAWTListener implements AWTEventListener
  {
    // VARIABLES

    private final SignalDiagramController controller;
    private JComponent compRoot;

    private volatile int movingCursor;

    volatile Timer resizeTimeout;

    // CONSTRUCTORS

    /**
     * @param aController
     */
    public TransparentAWTListener( final SignalDiagramController aController )
    {
      this.controller = aController;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void eventDispatched( final AWTEvent aEvent )
    {
      final int id = aEvent.getID();
      if ( aEvent instanceof MouseEvent )
      {
        final MouseEvent event = ( MouseEvent )aEvent;

        if ( !SwingUtilities.isDescendingFrom( event.getComponent(), getComponentRoot() ) )
        {
          // Do not process the mouse event in case it is not in our area of
          // interest...
          return;
        }

        if ( id == MouseEvent.MOUSE_CLICKED )
        {
          mouseClicked( event );
        }
        else if ( id == MouseEvent.MOUSE_PRESSED )
        {
          mousePressed( event );
        }
        else if ( id == MouseEvent.MOUSE_RELEASED )
        {
          mouseReleased( event );
        }
        else if ( id == MouseEvent.MOUSE_MOVED )
        {
          mouseMoved( event );
        }
        else if ( id == MouseEvent.MOUSE_DRAGGED )
        {
          mouseDragged( event );
        }
      }
      else if ( aEvent instanceof KeyEvent )
      {
        final KeyEvent event = ( KeyEvent )aEvent;

        if ( id == KeyEvent.KEY_PRESSED )
        {
          keyPressed( event );
        }
        else if ( id == KeyEvent.KEY_RELEASED )
        {
          keyReleased( event );
        }
      }
      else if ( aEvent instanceof ComponentEvent )
      {
        final ComponentEvent event = ( ComponentEvent )aEvent;
        final Component view = event.getComponent();
        // XXX the parent is the scrollpane's viewport???
        if ( view == SignalDiagramComponent.this.getParent() )
        {
          if ( id == ComponentEvent.COMPONENT_RESIZED )
          {
            componentResized( event );
          }
          else if ( id == ComponentEvent.COMPONENT_SHOWN )
          {
            componentShown( event );
          }
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void componentResized( final ComponentEvent aEvent )
    {
      if ( this.resizeTimeout == null )
      {
        this.resizeTimeout = new Timer( 250, new ActionListener()
        {
          @Override
          public void actionPerformed( final ActionEvent aInnerEvent )
          {
            final Component component = aEvent.getComponent();
            component.setCursor( CURSOR_WAIT );

            try
            {
              if ( isZoomAll() )
              {
                zoomAll();
              }
              else
              {
                recalculateDimensions();
              }
            }
            finally
            {
              component.setCursor( null );

              TransparentAWTListener.this.resizeTimeout.stop();
              TransparentAWTListener.this.resizeTimeout = null;
            }
          }
        } );

        this.resizeTimeout.setCoalesce( true );
        this.resizeTimeout.setRepeats( false );

        this.resizeTimeout.start();
      }
      else
      {
        this.resizeTimeout.restart();
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void componentShown( final ComponentEvent aEvent )
    {
      final Component component = aEvent.getComponent();
      component.setCursor( CURSOR_WAIT );

      try
      {
        if ( isZoomAll() )
        {
          zoomAll();
        }
        else
        {
          recalculateDimensions();
        }
      }
      finally
      {
        component.setCursor( java.awt.Cursor.getDefaultCursor() );
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void keyPressed( final KeyEvent aEvent )
    {
      Component comp = this.controller.getSignalDiagram();
      if ( isEdgeWarpTriggerModifier( aEvent ) )
      {
        comp.setCursor( CURSOR_MOVE_TIMESTAMP );
        // Consume this event...
        aEvent.consume();
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void keyReleased( final KeyEvent aEvent )
    {
      Component comp = this.controller.getSignalDiagram();
      comp.setCursor( null );
    }

    /**
     * {@inheritDoc}
     */
    protected void mouseClicked( final MouseEvent aEvent )
    {
      if ( isEdgeWarpTrigger( aEvent ) )
      {
        final JComponent view = getDeepestComponentAt( aEvent );
        final Point point = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), view );

        final MeasurementInfo signalHover = getModel().getSignalHover( point );
        if ( ( signalHover != null ) && !signalHover.isEmpty() )
        {
          final long timestamp = signalHover.getEndTimestamp().longValue();

          scrollToTimestamp( timestamp );
        }

        aEvent.consume();
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void mouseDragged( final MouseEvent aEvent )
    {
      if ( getModel().isCursorMode() && ( this.movingCursor >= 0 ) )
      {
        final JComponent view = getDeepestComponentAt( aEvent );
        final Point point = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), view );

        this.controller.moveCursor( this.movingCursor, getCursorDropPoint( point ) );
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void mouseMoved( final MouseEvent aEvent )
    {
      final JComponent view = getDeepestComponentAt( aEvent );

      final SignalDiagramModel model = getModel();
      if ( model.isCursorMode() || model.isMeasurementMode() )
      {
        final Point point = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), view );

        if ( model.isMeasurementMode() )
        {
          MeasurementInfo signalHover = model.getSignalHover( point );

          model.fireMeasurementEvent( signalHover );

          view.setCursor( signalHover != null ? CURSOR_HOVER : null );
        }

        if ( model.isCursorMode() )
        {
          view.setCursor( ( findCursor( point ) != null ) ? CURSOR_MOVE_CURSOR : null );
        }
      }
    }

    /**
     * @param aEvent
     */
    protected void mousePressed( final MouseEvent aEvent )
    {
      final JComponent view = getDeepestComponentAt( aEvent );
      if ( ( view == null ) || !SwingUtilities.isDescendingFrom( view, getComponentRoot() ) )
      {
        return;
      }
      final Point point = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), view );

      if ( !handlePopupTrigger( view, point, aEvent ) )
      {
        if ( getModel().isCursorMode() )
        {
          Cursor hoveredCursor = findCursor( point );
          this.movingCursor = ( hoveredCursor != null ) ? hoveredCursor.getIndex() : -1;
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    protected void mouseReleased( final MouseEvent aEvent )
    {
      final JComponent view = getDeepestComponentAt( aEvent );
      if ( ( view == null ) || !SwingUtilities.isDescendingFrom( view, getComponentRoot() ) )
      {
        return;
      }
      final Point point = SwingUtilities.convertPoint( aEvent.getComponent(), aEvent.getPoint(), view );

      view.setCursor( null );

      if ( !handlePopupTrigger( view, point, aEvent ) )
      {
        this.movingCursor = -1;
      }
    }

    /**
     * Creates the context-sensitive popup menu for channel labels.
     * 
     * @param aRelativePoint
     *          the current mouse location to show the popup menu, cannot be
     *          <code>null</code>.
     * @param aLocationOnScreen
     *          the location on screen, cannot be <code>null</code>.
     * @return a popup menu, can be <code>null</code> if the given mouse point
     *         is not above a channel.
     */
    private JPopupMenu createChannelLabelPopup( final Point aRelativePoint, final Point aLocationOnScreen )
    {
      final SignalElement signalElement = findSignalElement( aRelativePoint );
      if ( signalElement == null )
      {
        return null;
      }

      JPopupMenu result = new JPopupMenu();
      JMenuItem mi;

      mi = new JMenuItem( new EditSignalElementLabelAction( this.controller, signalElement, aLocationOnScreen ) );
      result.add( mi );

      result.addSeparator();

      mi = new JCheckBoxMenuItem( new SetSignalElementVisibilityAction( this.controller, signalElement ) );
      result.add( mi );

      if ( signalElement.isDigitalSignal() )
      {
        mi = new JMenuItem( new RemoveChannelAnnotations( this.controller, signalElement ) );
        result.add( mi );
      }

      if ( signalElement.isSignalGroup() )
      {
        // TODO add visibility actions for summary/analog signal/...
        mi = new JCheckBoxMenuItem( new SetSignalElementVisibilityAction( this.controller, signalElement ) );
        result.add( mi );
      }

      return result;
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
     * Finds the cursor under the given point.
     * 
     * @param aPoint
     *          the coordinate of the potential cursor, cannot be
     *          <code>null</code>.
     * @return the cursor index, or -1 if not found.
     */
    private Cursor findCursor( final Point aPoint )
    {
      return getModel().findCursor( aPoint );
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
      return getModel().findSignalElement( aPoint );
    }

    /**
     * @return
     */
    private JComponent getComponentRoot()
    {
      if ( this.compRoot == null )
      {
        this.compRoot = SwingComponentUtils.getAncestorOfClass( JScrollPane.class, SignalDiagramComponent.this );
        if ( this.compRoot == null )
        {
          this.compRoot = SignalDiagramComponent.this;
        }
      }
      return this.compRoot;
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
     * @param aEvent
     * @return
     */
    private JComponent getDeepestComponentAt( final MouseEvent aEvent )
    {
      JComponent view = SwingComponentUtils.getDeepestComponentAt( aEvent );
      if ( ( view != null ) && !SwingUtilities.isDescendingFrom( view, getComponentRoot() ) )
      {
        view = null;
      }
      return view;
    }

    /**
     * Determines whether or not the given mouse event is actually a popup
     * trigger.
     * 
     * @param aView
     *          the view to show to popup for, can be <code>null</code>;
     * @param aPoint
     *          the <em>corrected</em> mouse position, where the popup is to be
     *          shown, cannot be <code>null</code>;
     * @param aEvent
     *          the mouse event that could be a popup trigger, cannot be
     *          <code>null</code>.
     */
    private boolean handlePopupTrigger( final JComponent aView, final Point aPoint, final MouseEvent aEvent )
    {
      final boolean popupTrigger = isPopupTrigger( aEvent );
      if ( popupTrigger )
      {
        JPopupMenu contextMenu = null;
        if ( isCursorPopupTrigger( aView ) )
        {
          contextMenu = createCursorPopup( aPoint, aEvent.getLocationOnScreen() );
        }
        else if ( isChannelLabelPopupTrigger( aView ) )
        {
          contextMenu = createChannelLabelPopup( aPoint, aEvent.getLocationOnScreen() );
        }

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
     * Returns whether or not the 'edit channels' popup is to be shown.
     * 
     * @param aView
     *          the view to test, may be <code>null</code>.
     * @return <code>true</code> if the 'edit channel' popup is to be shown,
     *         <code>false</code> otherwise.
     */
    private boolean isChannelLabelPopupTrigger( final JComponent aView )
    {
      return ( aView instanceof ChannelLabelsView );
    }

    /**
     * Returns whether or not the 'edit cursor' popup is to be shown.
     * 
     * @param aView
     *          the view to test, may be <code>null</code>.
     * @return <code>true</code> if the 'edit cursor' popup is to be shown,
     *         <code>false</code> otherwise.
     */
    private boolean isCursorPopupTrigger( final JComponent aView )
    {
      return getModel().isCursorMode() && ( ( aView instanceof SignalView ) || ( aView instanceof TimeLineView ) );
    }

    /**
     * Returns whether or not the given mouse event denotes a "edge warp"
     * trigger event.
     * 
     * @param aEvent
     *          the mouse event to test, cannot be <code>null</code>.
     * @return <code>true</code> if the given mouse event represents a
     *         "edge warp" trigger event, <code>false</code> otherwise.
     */
    private boolean isEdgeWarpTrigger( final MouseEvent aEvent )
    {
      return !aEvent.isConsumed() && isEdgeWarpTriggerModifier( aEvent ) && ( aEvent.getClickCount() > 0 );
    }

    /**
     * Returns whether or not the given input event can be interpreted as a
     * 'edge warp' trigger event.
     * 
     * @param aEvent
     *          the input event to test, cannot be <code>null</code>.
     * @return <code>true</code> if the given input event is a 'edge warp'
     *         trigger event, <code>false</code> otherwise.
     */
    private boolean isEdgeWarpTriggerModifier( final InputEvent aEvent )
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
    private boolean isMacOS()
    {
      final String osName = System.getProperty( "os.name" );
      return ( "Mac OS X".equalsIgnoreCase( osName ) || "Darwin".equalsIgnoreCase( osName ) );
    }

    /**
     * Returns whether or not the given mouse event denotes a popup trigger
     * event.
     * 
     * @param aEvent
     *          the mouse event to test, cannot be <code>null</code>.
     * @return <code>true</code> if the given mouse event represents a popup
     *         trigger event, <code>false</code> otherwise.
     */
    private boolean isPopupTrigger( final MouseEvent aEvent )
    {
      return !aEvent.isConsumed() && aEvent.isPopupTrigger();
    }
  }

  /**
   * Action to zoom all via '0' key.
   */
  final class ZoomAllAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      zoomAll();
    }
  }

  /**
   * Defines a zoom factor, with a ratio and some additional properties.
   */
  static class ZoomHelper
  {
    // CONSTANTS

    private static final Logger LOG = Logger.getLogger( ZoomHelper.class.getName() );

    // VARIABLES

    private boolean zoomAll;

    private final SignalDiagramComponent signalDiagram;

    // CONSTRUCTORS

    /**
     * Creates a new SignalDiagramComponent.ZoomFactor instance.
     * 
     * @param aSignalDiagram
     *          the signal diagram component to use.
     */
    public ZoomHelper( final SignalDiagramComponent aSignalDiagram )
    {
      this.signalDiagram = aSignalDiagram;
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
     * Returns the current value of zoomAll.
     * 
     * @return the zoomAll
     */
    public boolean isZoomAll()
    {
      return this.zoomAll;
    }

    /**
     * Zooms to make all data visible in one screen.
     */
    public void zoomAll()
    {
      setFactor( getMinZoomLevel() );
      this.zoomAll = true;

      LOG.log( Level.INFO, "Zoom factor set to " + getFactor() );

      this.signalDiagram.recalculateDimensions();
    }

    /**
     * Zooms in with a factor 1.5
     */
    public void zoomIn()
    {
      zoomRelative( 2.0 );

      this.signalDiagram.recalculateDimensions();
    }

    /**
     * Zooms to a factor of 1.0.
     */
    public void zoomOriginal()
    {
      zoomAbsolute( 1.0 );

      this.signalDiagram.recalculateDimensions();
    }

    /**
     * Zooms out with a factor 1.5
     */
    public void zoomOut()
    {
      zoomRelative( 0.5 );

      this.signalDiagram.recalculateDimensions();
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
        return 1.0;
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
        return 1.0;
      }

      Rectangle viewSize = this.signalDiagram.getVisibleViewSize();
      final double length = model.getAbsoluteLength();

      return viewSize.getWidth() / length;
    }

    /**
     * @return
     */
    private SignalDiagramModel getModel()
    {
      return this.signalDiagram.getModel();
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

  /**
   * Action to zoom in via '+' or '=' key.
   */
  final class ZoomInAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      zoomIn();
    }
  }

  /**
   * Action to zoom to original via '1' key.
   */
  final class ZoomOriginalAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      zoomOriginal();
    }
  }

  /**
   * Action to zoom out via '-' or '_' key.
   */
  final class ZoomOutAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      zoomOut();
    }
  }

  // CONSTANTS

  static final java.awt.Cursor CURSOR_WAIT = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR );
  static final java.awt.Cursor CURSOR_HOVER = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.CROSSHAIR_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_CURSOR = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.MOVE_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_TIMESTAMP = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.E_RESIZE_CURSOR );

  private static final boolean DEBUG = false;

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalDiagramController controller;
  private final SignalView signalView;
  private final TransparentAWTListener awtListener;
  private final SignalDiagramModel model;
  private final ZoomHelper zoomHelper;

  // CONSTRUCTORS

  /**
   * Creates a new SampleViewComponent instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   */
  private SignalDiagramComponent( final SignalDiagramController aController )
  {
    super( new BorderLayout() );

    this.controller = aController;

    this.zoomHelper = new ZoomHelper( this );

    this.awtListener = new TransparentAWTListener( this.controller );
    this.model = new SignalDiagramModel( this.controller );
    this.signalView = SignalView.create( this.controller );

    add( this.signalView, BorderLayout.CENTER );
  }

  // METHODS

  /**
   * Factory method to create a new {@link SignalDiagramComponent} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDiagramComponent instance,
   *          cannot be <code>null</code>.
   * @return a new {@link SignalDiagramComponent} instance, never
   *         <code>null</code>.
   */
  public static SignalDiagramComponent create( final SignalDiagramController aController )
  {
    final SignalDiagramComponent result = new SignalDiagramComponent( aController );

    aController.setSignalDiagram( result );

    aController.addCursorChangeListener( result.getSignalView() );
    aController.addMeasurementListener( result.getSignalView() );

    return result;
  }

  /**
   * Installs all listeners and the support for DnD.
   * 
   * @see javax.swing.JComponent#addNotify()
   */
  @Override
  public void addNotify()
  {
    try
    {
      final long eventMask = AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK //
          | AWTEvent.KEY_EVENT_MASK | AWTEvent.COMPONENT_EVENT_MASK;
      Toolkit.getDefaultToolkit().addAWTEventListener( this.awtListener, eventMask );

      final GhostGlassPane glassPane = new GhostGlassPane( this.controller );
      final JRootPane rootPane = SwingUtilities.getRootPane( this );
      rootPane.setGlassPane( glassPane );

      configureEnclosingScrollPane();

      ZoomInAction zoomInAction = new ZoomInAction();
      ZoomOutAction zoomOutAction = new ZoomOutAction();

      registerKeyBinding( this, '+', zoomInAction );
      registerKeyBinding( this, '=', zoomInAction );
      registerKeyBinding( this, '-', zoomOutAction );
      registerKeyBinding( this, '_', zoomOutAction );
      registerKeyBinding( this, '0', new ZoomAllAction() );
      registerKeyBinding( this, '1', new ZoomOriginalAction() );
    }
    finally
    {
      super.addNotify();
    }
  }

  /**
   * Returns the current value of model.
   * 
   * @return the model, never <code>null</code>.
   */
  public final SignalDiagramModel getModel()
  {
    return this.model;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getScrollableBlockIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    final int inc;
    if ( aOrientation == SwingConstants.VERTICAL )
    {
      inc = this.model.getVerticalBlockIncrement( getSize(), aVisibleRect, aDirection );
    }
    else
    /* if ( aOrientation == SwingConstants.HORIZONTAL ) */
    {
      inc = this.model.getHorizontalBlockIncrement( aVisibleRect, aDirection );
    }

    return inc;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getScrollableUnitIncrement( final Rectangle aVisibleRect, final int aOrientation, final int aDirection )
  {
    return getScrollableBlockIncrement( aVisibleRect, aOrientation, aDirection );
  }

  /**
   * Returns the actual signal view component.
   * 
   * @return a signal view component, never <code>null</code>.
   */
  public final SignalView getSignalView()
  {
    return this.signalView;
  }

  /**
   * Returns the dimensions of the visible view, taking care of viewports (such
   * as used in {@link JScrollPane}).
   * 
   * @return a visible view size, as {@link Dimension}, never <code>null</code>.
   */
  public final Rectangle getVisibleViewSize()
  {
    final JComponent component = getSignalView();

    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, component );

    final Rectangle rect;
    if ( scrollPane != null )
    {
      rect = scrollPane.getViewport().getVisibleRect();
    }
    else
    {
      rect = getVisibleRect();
    }

    return rect;
  }

  /**
   * @see javax.swing.JComponent#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics aGraphics )
  {
    if ( DEBUG )
    {
      final long startTime = System.nanoTime();
      try
      {
        super.paint( aGraphics );
      }
      finally
      {
        final long endTime = System.nanoTime();
        final long renderTime = endTime - startTime;
        System.out.println( "Rendering time = " + displayTime( renderTime / 1.0e9 ) );
      }
    }
    else
    {
      super.paint( aGraphics );
    }
  }

  /**
   * Recalculates the dimensions of the main view.
   */
  public void recalculateDimensions()
  {
    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, getSignalView() );
    if ( scrollPane == null )
    {
      return;
    }

    final Rectangle viewPortSize = scrollPane.getViewport().getVisibleRect();

    int width = this.model.getAbsoluteScreenWidth();
    if ( width < viewPortSize.width )
    {
      width = viewPortSize.width;
    }

    int height = this.model.getAbsoluteScreenHeight();
    if ( height < viewPortSize.height )
    {
      height = viewPortSize.height;
    }

    JComponent signalView = ( JComponent )scrollPane.getViewport().getView();
    signalView.setPreferredSize( new Dimension( width, height ) );
    signalView.revalidate();

    TimeLineView timeline = ( TimeLineView )scrollPane.getColumnHeader().getView();
    // the timeline component always follows the width of the signal view, but
    // with a fixed height...
    timeline.setPreferredSize( new Dimension( width, timeline.getTimeLineHeight() ) );
    timeline.setMinimumSize( signalView.getPreferredSize() );
    timeline.revalidate();

    ChannelLabelsView channelLabels = ( ChannelLabelsView )scrollPane.getRowHeader().getView();
    // the channel label component calculates its own 'optimal' width, but
    // doesn't know squat about the correct height...
    final Dimension minimumSize = channelLabels.getMinimumSize();
    channelLabels.setPreferredSize( new Dimension( minimumSize.width, height ) );
    channelLabels.revalidate();

    scrollPane.repaint();
  }

  /**
   * @see javax.swing.JComponent#removeNotify()
   */
  @Override
  public void removeNotify()
  {
    try
    {
      unconfigureEnclosingScrollPane();

      Toolkit.getDefaultToolkit().removeAWTEventListener( this.awtListener );
    }
    finally
    {
      super.removeNotify();
    }
  }

  /**
   * Repaints the area of this component specified by the given
   * {@link SignalElement}.
   * 
   * @param aSignalElement
   *          the signal element to repaint, cannot be <code>null</code>.
   */
  public void repaintSignalElement( final SignalElement aSignalElement )
  {
    final Rectangle rect = getVisibleRect();
    rect.y = aSignalElement.getYposition();
    rect.height = aSignalElement.getHeight();

    repaint( rect );
  }

  /**
   * Scrolls the signal diagram component so that the given timestamp for the
   * given channel becomes visible.
   * 
   * @param aTimestamp
   *          the timestamp to make visible, >= 0 and < last timestamp.
   */
  public void scrollToTimestamp( final long aTimestamp )
  {
    final SignalView signalView = getSignalView();
    final Rectangle visibleRect = signalView.getVisibleRect();

    Rectangle rect = new Rectangle();
    rect.width = visibleRect.width;
    rect.height = visibleRect.height;
    rect.x = ( int )( ( getModel().getZoomFactor() * aTimestamp ) - rect.getCenterX() );
    rect.y = visibleRect.y;

    signalView.scrollRectToVisible( rect );
  }

  /**
   * Zooms to make all data visible in one screen.
   */
  public final void zoomAll()
  {
    this.zoomHelper.zoomAll();
  }

  /**
   * Zooms in with a factor 1.5
   */
  public final void zoomIn()
  {
    this.zoomHelper.zoomIn();
  }

  /**
   * Zooms to a factor of 1.0.
   */
  public final void zoomOriginal()
  {
    this.zoomHelper.zoomOriginal();
  }

  /**
   * Zooms out with a factor 1.5
   */
  public final void zoomOut()
  {
    this.zoomHelper.zoomOut();
  }

  /**
   * @return
   */
  final boolean isZoomAll()
  {
    return this.zoomHelper.isZoomAll();
  }

  /**
   * If this component is the <code>viewportView</code> of an enclosing
   * <code>JScrollPane</code> (the usual situation), configure this
   * <code>ScrollPane</code> by, amongst other things, installing the diagram's
   * <code>timeline</code> as the <code>columnHeaderView</code> of the scroll
   * pane.
   * 
   * @see #addNotify
   */
  private void configureEnclosingScrollPane()
  {
    JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, this );
    if ( scrollPane != null )
    {
      // Make certain we are the viewPort's view and not, for
      // example, the rowHeaderView of the scrollPane -
      // an implementor of fixed columns might do this.
      final JViewport viewport = scrollPane.getViewport();
      if ( ( viewport == null ) || ( viewport.getView() != this ) )
      {
        return;
      }

      final TimeLineView timelineView = TimeLineView.create( this.controller );
      scrollPane.setColumnHeaderView( timelineView );

      final ChannelLabelsView channelLabelsView = ChannelLabelsView.create( this.controller );
      scrollPane.setRowHeaderView( channelLabelsView );

      scrollPane.setCorner( ScrollPaneConstants.UPPER_LEADING_CORNER, new CornerView( this.controller ) );
    }
  }

  /**
   * Reverses the effect of <code>configureEnclosingScrollPane</code> by
   * replacing the <code>columnHeaderView</code> of the enclosing scroll pane
   * with <code>null</code>.
   * 
   * @see #removeNotify
   * @see #configureEnclosingScrollPane
   */
  private void unconfigureEnclosingScrollPane()
  {
    JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, this );
    if ( scrollPane != null )
    {
      scrollPane.setColumnHeaderView( null );
      scrollPane.setRowHeaderView( null );
      scrollPane.setCorner( ScrollPaneConstants.UPPER_LEADING_CORNER, null );
    }
  }
}
