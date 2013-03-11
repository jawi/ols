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
package nl.lxtreme.ols.client.ui.signaldisplay.view.timeline;


import java.awt.*;
import java.awt.event.*;

import nl.lxtreme.ols.client.ui.signaldisplay.*;
import nl.lxtreme.ols.client.ui.signaldisplay.marker.*;
import nl.lxtreme.ols.client.ui.signaldisplay.view.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a time line view, displaying ticks at regular intervals along with
 * timing information.
 */
public class TimeLineView extends AbstractViewLayer implements IMarkerChangeListener, IDataModelChangeListener
{
  // INNER TYPES

  /**
   * Provides a concrete mouse handler for this view.
   */
  private final class MouseHandler extends AbstractMouseHandler
  {
    // CONSTRUCTORS

    /**
     * Creates a new MouseHandler instance.
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
      if ( model.isCursorMode() )
      {
        final Marker hoveredMarker = findMarker( point );
        if ( hoveredMarker != null )
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

      setToolTipText( ViewUtils.getToolTipText( getModel(), point ) );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final TimeLineViewModel model;
  private final MouseHandler mouseHandler;

  // CONSTRUCTORS

  /**
   * Creates a new {@link TimeLineView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   */
  private TimeLineView( final SignalDiagramController aController )
  {
    super( aController );

    this.model = new TimeLineViewModel( aController );

    this.mouseHandler = new MouseHandler( aController );

    updateUI();
  }

  // METHODS

  /**
   * Factory method for creating new {@link TimeLineView} instances.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   * @return a {@link TimeLineView} instance, never <code>null</code>.
   */
  public static TimeLineView create( final SignalDiagramController aController )
  {
    TimeLineView result = new TimeLineView( aController );

    aController.addCursorChangeListener( result );
    aController.addDataModelChangeListener( result );

    return result;
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
  public void dataModelChanged( final AcquisitionData aData )
  {
    repaint( 50L );
  }

  /**
   * @return a {@link TimeLineViewModel} instance, never <code>null</code>.
   */
  public TimeLineViewModel getModel()
  {
    return this.model;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerAdded( final Marker aCursor )
  {
    repaint( 50L );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerChanged( final String aPropertyName, final Marker aNewCursor )
  {
    repaint( 50L );
  }

  /**
   * {@inheritDoc}
   */
  public void markerMoved( final long aOldTimestamp, final long aNewTimestamp )
  {
    repaint( 50L );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void markerRemoved( final Marker aOldCursor )
  {
    repaint( 50L );
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
    setUI( new TimeLineUI() );
  }
}
