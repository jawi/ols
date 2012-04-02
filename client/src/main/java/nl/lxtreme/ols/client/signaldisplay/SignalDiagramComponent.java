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


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.signaldisplay.ZoomController.ZoomEvent;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a signal diagram, where signals in the form of sample data is
 * represented by channels.
 */
public class SignalDiagramComponent extends JPanel implements Scrollable
{
  // INNER TYPES

  /**
   * Handles resizing of this component.
   */
  static final class ComponentEventHandler extends ComponentAdapter
  {
    // VARIABLES

    private final SignalDiagramController controller;
    private final AccumulatingRunnable<ComponentEvent> repaintQueue;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ComponentEventHandler} instance.
     */
    public ComponentEventHandler( final SignalDiagramController aController )
    {
      this.controller = aController;
      this.repaintQueue = new AccumulatingRunnable<ComponentEvent>()
      {
        @Override
        protected void run( final Deque<ComponentEvent> aArguments )
        {
          scheduleRedraw( aArguments.getLast() );
        }
      };
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void componentResized( final ComponentEvent aEvent )
    {
      this.repaintQueue.add( aEvent );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void componentShown( final ComponentEvent aEvent )
    {
      this.repaintQueue.add( aEvent );
    }

    /**
     * Schedules a repaint for the entire component.
     * 
     * @param aEvent
     *          a component event to use, cannot be <code>null</code>.
     */
    final void scheduleRedraw( final ComponentEvent aEvent )
    {
      final JRootPane component = SwingUtilities.getRootPane( aEvent.getComponent() );
      component.setCursor( CURSOR_WAIT );

      try
      {
        final ZoomController zoomCtrl = this.controller.getZoomController();
        if ( zoomCtrl.isZoomAll() )
        {
          zoomCtrl.zoomAll();
        }
        else
        {
          zoomCtrl.restoreZoomLevel();
        }
      }
      finally
      {
        component.setCursor( null );
      }
    }
  }

  /**
   * Provides a global key listener for listening to certain modifier keys.
   */
  final class GlobalKeyListener implements KeyEventDispatcher
  {
    /**
     * {@inheritDoc}
     */
    @Override
    public boolean dispatchKeyEvent( final KeyEvent aEvent )
    {
      if ( isTimestampWarpKeyEvent( aEvent ) )
      {
        setCursor( CURSOR_MOVE_TIMESTAMP );
      }
      else
      {
        setCursor( null );
      }
      return false;
    }

    /**
     * @param aEvent
     * @return
     */
    boolean isTimestampWarpKeyEvent( final KeyEvent aEvent )
    {
      if ( !hasData() )
      {
        return false;
      }
      if ( !SwingComponentUtils.isActivelyShown( SignalDiagramComponent.this ) )
      {
        return false;
      }
      return SignalView.isTimestampWarpModifier( aEvent );
    }
  }

  // CONSTANTS

  static final java.awt.Cursor CURSOR_WAIT = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR );
  static final java.awt.Cursor CURSOR_HOVER = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.CROSSHAIR_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_CURSOR = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.HAND_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_TIMESTAMP = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.E_RESIZE_CURSOR );

  private static final boolean DEBUG = true;

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalDiagramController controller;

  private final SignalView signalView;

  private final ComponentEventHandler componentHandler;

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

    this.signalView = SignalView.create( this.controller );

    this.componentHandler = new ComponentEventHandler( this.controller );

    add( this.signalView, BorderLayout.CENTER );

    setOpaque( false );
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
    final SignalDiagramModel model = new SignalDiagramModel( aController );
    aController.setSignalDiagramModel( model );

    // Register our controller as listener for zooming events...
    model.getZoomController().addZoomListener( aController );

    final SignalDiagramComponent result = new SignalDiagramComponent( aController );
    aController.setSignalDiagram( result );

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
      final GhostGlassPane glassPane = new GhostGlassPane( this.controller );
      final JRootPane rootPane = SwingUtilities.getRootPane( this );
      rootPane.setGlassPane( glassPane );

      configureEnclosingScrollPane();

      final IActionManager actionManager = this.controller.getActionManager();

      // Wrap the original zoom-actions so that we can update its state upon
      // each invocation...
      Action zoomInAction = actionManager.getAction( ZoomInAction.ID );
      Action zoomOutAction = actionManager.getAction( ZoomOutAction.ID );
      Action zoomAllAction = actionManager.getAction( ZoomAllAction.ID );
      Action zoomOriginalAction = actionManager.getAction( ZoomOriginalAction.ID );

      registerKeyBinding( this, '+', zoomInAction );
      registerKeyBinding( this, '=', zoomInAction );
      registerKeyBinding( this, '-', zoomOutAction );
      registerKeyBinding( this, '_', zoomOutAction );
      registerKeyBinding( this, '0', zoomAllAction );
      registerKeyBinding( this, '1', zoomOriginalAction );

      KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher( new GlobalKeyListener() );

      revalidate();
    }
    finally
    {
      super.addNotify();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void doLayout()
  {
    calculateDimensions();
    super.doLayout();
  }

  /**
   * Returns the model of this component.
   * 
   * @return the model, never <code>null</code>.
   */
  public final SignalDiagramModel getModel()
  {
    return this.controller.getSignalDiagramModel();
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
      inc = getModel().getVerticalBlockIncrement( getSize(), aVisibleRect, aDirection );
    }
    else
    /* if ( aOrientation == SwingConstants.HORIZONTAL ) */
    {
      inc = getModel().getHorizontalBlockIncrement( aVisibleRect, aDirection );
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
        System.out.println( "Rendering time = " + UnitOfTime.NS.format( 1.0 / renderTime, 2 ) );
      }
    }
    else
    {
      super.paint( aGraphics );
    }
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
   * Returns whether or not there's data to display.
   * 
   * @return <code>true</code> if there is any captured data to display,
   *         <code>false</code> otherwise.
   */
  final boolean hasData()
  {
    return getModel().hasData();
  }

  /**
   * @param aEvent
   */
  final void notifyZoomChange( final ZoomEvent aEvent )
  {
    final SignalDiagramModel model = getModel();
    if ( !model.hasData() )
    {
      return;
    }

    // Idea based on <http://stackoverflow.com/questions/115103>

    final SignalView view = getSignalView();

    double mx;
    double my = 0;

    Point hotSpot = aEvent.getHotSpot();
    if ( hotSpot != null )
    {
      mx = hotSpot.x;
    }
    else
    {
      mx = view.getVisibleRect().getCenterX();
    }

    final double newZf = aEvent.getFactor();
    final double oldZf = aEvent.getOldFactor();
    final double relZf = newZf / oldZf;

    System.out.println( "notifyZoomChange[" + view.getClass().getSimpleName() + "] " + hotSpot + "; "
        + view.getVisibleRect().getCenterX() + "; " + relZf );

    if ( hotSpot == null )
    {
      // Calculate the timestamp from the center position of the visible view
      // rectangle; after which we lookup the exact timestamp that is at that
      // position. If found, we'll use that timestamp to recalculate the new
      // center position in the new zoom factor...
      final long timestamp = ( long )Math.ceil( mx / oldZf );
      final int tsIdx = model.getTimestampIndex( timestamp );

      mx = Math.floor( model.getTimestamps()[tsIdx] * oldZf );
      System.out.println( "mx (ts) = " + mx );
    }

    if ( aEvent.isFactorChange() )
    {
      // Recalculate all dimensions...
      calculateDimensions();
    }

    final Point location = getLocation();

    // Recalculate the new screen position of the visible view rectangle...
    int newX = ( int )( location.getX() - ( ( mx * relZf ) - mx ) );
    int newY = ( int )( location.getY() - ( ( my * relZf ) - my ) );

    setLocation( newX, newY );

    // Notify that everything needs to be revalidated as well...
    revalidateAll();
  }

  /**
   * Calculates all dimensions of the contained components.
   */
  private void calculateDimensions()
  {
    final SignalView view = getSignalView();
    final Dimension frameSize = getRootPane().getSize();
    final SignalDiagramModel model = getModel();

    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, view );
    if ( scrollPane != null )
    {
      final Rectangle viewPortSize = scrollPane.getViewport().getVisibleRect();

      TimeLineView timeline = ( TimeLineView )scrollPane.getColumnHeader().getView();
      ChannelLabelsView channelLabels = ( ChannelLabelsView )scrollPane.getRowHeader().getView();

      final int clWidth = channelLabels.getPreferredWidth();
      final int tlHeight = timeline.getTimeLineHeight();

      final int minWidth = Math.max( viewPortSize.width, frameSize.width - clWidth );

      final int newWidth = Math.max( minWidth, model.getAbsoluteScreenWidth() );
      final int newHeight = Math.max( viewPortSize.height, model.getAbsoluteScreenHeight() );

      // the timeline component always follows the width of the signal view, but
      // with a fixed height...
      timeline.setPreferredSize( new Dimension( newWidth, tlHeight ) );

      // the channel label component calculates its own 'optimal' width, but
      // doesn't know squat about the correct height...
      channelLabels.setPreferredSize( new Dimension( clWidth, newHeight ) );

      view.setPreferredSize( new Dimension( newWidth, newHeight ) );
    }
    else
    {
      final Rectangle viewSize = view.getVisibleRect();

      final int minWidth = Math.max( viewSize.width, frameSize.width );

      final int newWidth = Math.max( minWidth, model.getAbsoluteScreenWidth() );
      final int newHeight = Math.max( viewSize.height, model.getAbsoluteScreenHeight() );

      view.setPreferredSize( new Dimension( newWidth, newHeight ) );
    }
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

      scrollPane.addComponentListener( this.componentHandler );
    }
  }

  /**
   * Revalidates this component, the timeline and channel labels.
   */
  private void revalidateAll()
  {
    this.signalView.revalidate();

    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, this );
    if ( scrollPane != null )
    {
      TimeLineView timeline = ( TimeLineView )scrollPane.getColumnHeader().getView();
      timeline.revalidate();

      ChannelLabelsView channelLabels = ( ChannelLabelsView )scrollPane.getRowHeader().getView();
      channelLabels.revalidate();
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
      scrollPane.removeComponentListener( this.componentHandler );

      scrollPane.setColumnHeaderView( null );
      scrollPane.setRowHeaderView( null );
      scrollPane.setCorner( ScrollPaneConstants.UPPER_LEADING_CORNER, null );
    }
  }
}
