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
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.actionmanager.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.view.*;
import nl.lxtreme.ols.util.*;


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
        zoomCtrl.restoreZoomLevel();
      }
      finally
      {
        component.setCursor( null );
      }
    }
  }

  // CONSTANTS

  static final java.awt.Cursor CURSOR_WAIT = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.WAIT_CURSOR );
  static final java.awt.Cursor CURSOR_HOVER = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.CROSSHAIR_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_CURSOR = java.awt.Cursor.getPredefinedCursor( java.awt.Cursor.HAND_CURSOR );
  static final java.awt.Cursor CURSOR_MOVE_TIMESTAMP = java.awt.Cursor
      .getPredefinedCursor( java.awt.Cursor.E_RESIZE_CURSOR );

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
  SignalDiagramComponent( final SignalDiagramController aController )
  {
    super( new BorderLayout() );

    this.controller = aController;

    this.signalView = SignalView.create( this.controller );

    this.componentHandler = new ComponentEventHandler( this.controller );

    add( this.signalView, BorderLayout.CENTER );

    setOpaque( false );
    // Enable synthetic drag events (even when mouse is outside window)...
    setAutoscrolls( true );
    // We can receive the focus...
    setFocusable( true );
    setRequestFocusEnabled( true );
  }

  // METHODS

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

      registerKeyBindings();

      revalidate();
    }
    finally
    {
      super.addNotify();
    }
  }

  /**
   * Returns the model of this component.
   * 
   * @return the model, never <code>null</code>.
   */
  public final SignalDiagramModel getModel()
  {
    return this.controller.getViewModel();
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
  @Override
  public Rectangle getVisibleRect()
  {
    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, this );

    final Rectangle rect;
    if ( scrollPane != null )
    {
      rect = scrollPane.getViewport().getViewRect();
    }
    else
    {
      rect = super.getVisibleRect();
    }

    return rect;
  }

  /**
   * @see javax.swing.JComponent#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics aGraphics )
  {
    if ( Activator.isDebugMode() )
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
        System.out.printf( "Rendering time = %s, View = %s.%n", Unit.Time.format( renderTime / 1.0e9 ),
            getVisibleRect() );
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
   * Repaints this component, the timeline and channel labels.
   */
  final void repaintAll()
  {
    this.signalView.repaint( 50L );

    final JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, this );
    if ( scrollPane != null )
    {
      TimeLineView timeline = ( TimeLineView )scrollPane.getColumnHeader().getView();
      timeline.repaint( 50L );

      ChannelLabelsView channelLabels = ( ChannelLabelsView )scrollPane.getRowHeader().getView();
      channelLabels.repaint( 50L );
    }
  }

  /**
   * Revalidates this component, the timeline and channel labels.
   */
  final void revalidateAll()
  {
    revalidate();

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
   * Registers all key bindings this component supports.
   */
  private void registerKeyBindings()
  {
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
    registerKeyBinding( this, '[', zoomAllAction );
    registerKeyBinding( this, ']', zoomOriginalAction );

    int[] modifiers = { InputEvent.SHIFT_DOWN_MASK, InputEvent.ALT_DOWN_MASK, InputEvent.CTRL_DOWN_MASK,
        InputEvent.META_DOWN_MASK };

    Action smartJumpLeftAction = actionManager.getAction( SmartJumpAction.getJumpLeftID() );

    int[] smartJumpLeftKeys = { KeyEvent.VK_LEFT, KeyEvent.VK_KP_LEFT };
    for ( int key : smartJumpLeftKeys )
    {
      for ( int modifier : modifiers )
      {
        registerKeyBinding( this, KeyStroke.getKeyStroke( key, modifier ), smartJumpLeftAction );
      }
    }

    Action smartJumpRightAction = actionManager.getAction( SmartJumpAction.getJumpRightID() );

    int[] smartJumpRightKeys = { KeyEvent.VK_RIGHT, KeyEvent.VK_KP_RIGHT };
    for ( int key : smartJumpRightKeys )
    {
      for ( int modifier : modifiers )
      {
        registerKeyBinding( this, KeyStroke.getKeyStroke( key, modifier ), smartJumpRightAction );
      }
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
