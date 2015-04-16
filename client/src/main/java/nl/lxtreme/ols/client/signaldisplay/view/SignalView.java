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

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;


/**
 * Provides a view for the signal data as individual channels.
 */
public class SignalView extends AbstractViewLayer implements IMeasurementListener, ICursorChangeListener
{
  // INNER TYPES

  /**
   * Provides a concrete mouse handler for this view.
   */
  private final class MouseHandler extends AbstractMouseHandler
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

      // Update the selected channel while moving...
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

        SignalElement element = findDigitalSignal( point );
        if ( element != null )
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
    private SignalElement findDigitalSignal( final Point aPoint )
    {
      IUIElement element = this.controller.getViewModel().findUIElement( aPoint );
      if ( element instanceof SignalElement && ( ( SignalElement )element ).isDigitalSignal() )
      {
        return ( SignalElement )element;
      }
      return null;
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
  private final AbstractMouseHandler mouseHandler;

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
    final Rectangle visibleRect = getVisibleRect();
    final int y = visibleRect.y;
    final int height = visibleRect.height;

    final SignalViewModel model = getModel();

    int cursorPos = model.timestampToCoordinate( aOldCursor.getTimestamp() );
    repaint( 0, cursorPos - 1, y, 2, height );

    cursorPos = model.timestampToCoordinate( aNewCursor.getTimestamp() );
    repaint( 0, cursorPos - 1, y, 2, height );
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
    return getController().getViewModel();
  }
}
