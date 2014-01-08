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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a waveform view of {@link AcquisitionData}, in which data is shown
 * graphically as rows.
 */
public class WaveformView extends BaseView
{
  // INNER TYPES

  /**
   * Handles mouse events from the various components.
   */
  final class ViewMouseListener extends MouseAdapter
  {
    // VARIABLES

    private volatile Cursor movingCursor = null;
    private volatile Point lastClickPosition = null;

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseClicked( final MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;

      if ( aEvent.getComponent() == mainComponent || aEvent.getComponent() == timelineComponent )
      {
        if ( aEvent.getClickCount() == 2 )
        {
          Cursor hoveredCursor = model.getSelectedCursor();
          if ( model.areCursorsVisible() && ( hoveredCursor != null ) )
          {
            editCursorProperties( hoveredCursor );
            // Consume the event to stop further processing...
            aEvent.consume();
          }
          else
          {
            // Regular zoom event...
            MouseEvent event = convertEvent( aEvent );
            Point point = event.getPoint();

            int rotation = ( aEvent.isAltDown() || aEvent.isShiftDown() ) ? 1 : -1;

            model.zoom( rotation, point );
            // Consume the event to stop further processing...
            aEvent.consume();
          }
        }
      }
      else if ( aEvent.getComponent() == labelComponent )
      {
        if ( aEvent.getClickCount() == 2 )
        {
          WaveformElement hoveredElement = model.getSelectedElement();
          if ( hoveredElement != null )
          {
            editElementProperties( hoveredElement );
            // Consume the event to stop further processing...
            aEvent.consume();
          }
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseDragged( MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      // Update the selected element...
      updateSelectedElement( model, point );

      if ( model.areCursorsVisible() && ( this.movingCursor != null ) )
      {
        long newTimestamp = model.coordinateToTimestamp( point );

        repaintCursor( this.movingCursor );

        this.movingCursor.setTimestamp( newTimestamp );

        repaintCursor( this.movingCursor );

        // We're done with the given event...
        aEvent.consume();
        return;
      }

      boolean button1down = ( aEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK ) != 0;
      boolean horizontalOnly = ( aEvent.getModifiersEx() & InputEvent.ALT_DOWN_MASK ) != 0;
      boolean verticalOnly = horizontalOnly && ( ( aEvent.getModifiersEx() & InputEvent.SHIFT_DOWN_MASK ) != 0 );

      if ( button1down )
      {
        if ( this.lastClickPosition == null )
        {
          this.lastClickPosition = new Point( point );
        }
        else
        {
          Point _clickPos = this.lastClickPosition;

          JScrollPane scrollPane = getAncestorOfClass( JScrollPane.class, aEvent.getComponent() );
          if ( scrollPane != null )
          {
            int dx = aEvent.getX() - _clickPos.x;
            int dy = aEvent.getY() - _clickPos.y;

            JViewport viewPort = scrollPane.getViewport();
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

            int diagramWidth = mainComponent.getWidth();
            int viewportWidth = viewPort.getWidth();
            int maxX = diagramWidth - viewportWidth - 1;
            scrollPosition.x = Math.max( 0, Math.min( maxX, newX ) );

            int diagramHeight = mainComponent.getHeight();
            int viewportHeight = viewPort.getHeight();
            int maxY = diagramHeight - viewportHeight;
            scrollPosition.y = Math.max( 0, Math.min( maxY, newY ) );

            viewPort.setViewPosition( scrollPosition );
          }
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseMoved( MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      updateSelectedElement( model, point );

      Cursor newCursor = updateSelectedCursor( model, point );

      setCursor( null );
      if ( model.areCursorsVisible() && ( newCursor != null ) )
      {
        setCursor( CURSOR_MOVE_CURSOR );
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mousePressed( MouseEvent aEvent )
    {
      WaveformModel model = ( WaveformModel )WaveformView.this.model;
      MouseEvent event = convertEvent( aEvent );
      Point point = event.getPoint();

      this.movingCursor = model.getSelectedCursor();

      if ( ( aEvent.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK ) != 0 )
      {
        this.lastClickPosition = point;
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseReleased( MouseEvent aEvent )
    {
      setCursor( null );
      this.movingCursor = null;
    }

    private MouseEvent convertEvent( final MouseEvent aEvent )
    {
      JComponent view = SwingComponentUtils.getDeepestComponentAt( aEvent );
      return SwingUtilities.convertMouseEvent( aEvent.getComponent(), aEvent, view );
    }

    private Cursor updateSelectedCursor( WaveformModel model, Point point )
    {
      Cursor oldCursor = model.getSelectedCursor();
      Cursor newCursor = model.findCursor( point );

      if ( newCursor != oldCursor )
      {
        model.setSelectedCursor( newCursor );

        // Repaint the affected areas
        if ( newCursor != null )
        {
          repaintCursor( newCursor );
        }
        if ( oldCursor != null )
        {
          repaintCursor( oldCursor );
        }
      }

      return newCursor;
    }

    private void updateSelectedElement( WaveformModel model, Point point )
    {
      WaveformElement oldElement = model.getSelectedElement();
      WaveformElement newElement = model.findWaveformElement( point );

      if ( newElement != oldElement )
      {
        model.setSelectedElement( newElement );

        // Repaint the affected areas
        if ( newElement != null )
        {
          repaintWaveformLabel( newElement );
        }
        if ( oldElement != null )
        {
          repaintWaveformLabel( oldElement );
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

  private WaveformTimelineComponent timelineComponent;
  private WaveformLabelComponent labelComponent;
  private WaveformViewComponent mainComponent;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>.
   */
  public WaveformView( ViewController aController, ViewModel aModel )
  {
    super( aController, aModel );
  }

  // METHODS

  /**
   * Returns the main view component.
   * 
   * @return a view component, can only be <code>null</code> if this view itself
   *         is not initialized.
   */
  public WaveformViewComponent getViewComponent()
  {
    return this.mainComponent;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void initialize()
  {
    ViewMouseListener mouseListener = new ViewMouseListener();

    this.mainComponent = new WaveformViewComponent( ( WaveformModel )this.model );
    this.mainComponent.addMouseListener( mouseListener );
    this.mainComponent.addMouseMotionListener( mouseListener );
    this.timelineComponent = new WaveformTimelineComponent( ( WaveformModel )this.model );
    this.timelineComponent.addMouseListener( mouseListener );
    this.timelineComponent.addMouseMotionListener( mouseListener );
    this.labelComponent = new WaveformLabelComponent( ( WaveformModel )this.model );
    this.labelComponent.addMouseListener( mouseListener );
    this.labelComponent.addMouseMotionListener( mouseListener );

    JPopupMenu popup = new JPopupMenu();
    popup.addPopupMenuListener( new PopupMenuListener()
    {
      @Override
      public void popupMenuCanceled( PopupMenuEvent aEvent )
      {
        JPopupMenu popup = ( JPopupMenu )aEvent.getSource();
        popup.removeAll();
      }

      @Override
      public void popupMenuWillBecomeInvisible( PopupMenuEvent aEvent )
      {
        JPopupMenu popup = ( JPopupMenu )aEvent.getSource();
        popup.removeAll();
      }

      @Override
      public void popupMenuWillBecomeVisible( PopupMenuEvent aEvent )
      {
        JPopupMenu popup = ( JPopupMenu )aEvent.getSource();
        JComponent invoker = ( JComponent )popup.getInvoker();
        populatePopup( popup, invoker );
      }
    } );
    setComponentPopupMenu( popup );

    JScrollPane scrollPane = new JScrollPane( this.mainComponent );
    scrollPane.setColumnHeaderView( this.timelineComponent );
    scrollPane.setRowHeaderView( this.labelComponent );
    scrollPane.setCorner( ScrollPaneConstants.UPPER_LEFT_CORNER, new WaveformCornerComponent() );
    scrollPane.setInheritsPopupMenu( true );

    add( scrollPane, BorderLayout.CENTER );

    registerKeyBindings();
  }

  final void editCursorProperties( Cursor aCursor )
  {
    // TODO
  }

  final void editElementProperties( WaveformElement aElement )
  {
    // TODO
  }

  /**
   * Populates the popup according to the context of the given invoker.
   * 
   * @param aPopup
   * @param aInvoker
   */
  final void populatePopup( JPopupMenu aPopup, JComponent aInvoker )
  {
    if ( aInvoker instanceof WaveformViewComponent )
    {
      aPopup.add( new JMenuItem( "Edit session title..." ) );
    }
    else if ( aInvoker instanceof WaveformTimelineComponent )
    {

    }
    else if ( aInvoker instanceof WaveformLabelComponent )
    {

    }
    else
    {
      System.out.println( "Unknown invoker: " + aInvoker );
    }
  }

  /**
   * Repaints the area taken up by the given cursor on screen.
   * 
   * @param aCursor
   *          the cursor to repaint, cannot be <code>null</code>.
   */
  final void repaintCursor( Cursor aCursor )
  {
    WaveformModel model = ( WaveformModel )this.model;

    this.timelineComponent.repaint(); // TODO

    Rectangle rect = this.mainComponent.getVisibleRect();
    rect.x = model.timestampToCoordinate( aCursor.getTimestamp() ) - 1;
    rect.width = 2;

    this.mainComponent.repaint( rect );
  }

  /**
   * Repaints the area taken up by the given waveform element on screen.
   * 
   * @param aElement
   *          the waveform element to repaint, cannot be <code>null</code>.
   */
  final void repaintWaveformLabel( WaveformElement aElement )
  {
    Rectangle rect = this.labelComponent.getVisibleRect();

    rect.y = aElement.getYposition();
    rect.height = aElement.getHeight();
    this.labelComponent.repaint( rect );

    WaveformModel model = ( WaveformModel )this.model;
    int groupIdx = aElement.getGroupIndex();
    for ( WaveformElement element : model.getWaveformElements() )
    {
      if ( Type.GROUP.equals( element.getType() ) && groupIdx == element.getGroupIndex() )
      {
        rect.y = element.getYposition();
        rect.height = element.getHeight();
        this.labelComponent.repaint( rect );
      }
    }
  }

  /**
   * Registers all key bindings this component supports.
   */
  private void registerKeyBindings()
  {
    ActionManager actionManager = this.controller.getActionManager();

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
}
