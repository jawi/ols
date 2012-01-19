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


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.cursor.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;


/**
 * Provides a time line view, displaying ticks at regular intervals along with
 * timing information.
 */
public class TimeLineView extends AbstractViewLayer implements ICursorChangeListener, IDataModelChangeListener
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final TimeLineViewModel model;

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
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorAdded( final Cursor aCursor )
  {
    // final int visibleHeight = getVisibleRect().height;
    //
    // final TimeLineViewModel model = getModel();
    //
    // final long timestamp = aCursor.getTimestamp();
    //
    // int cursorPos = model.timestampToCoordinate( timestamp );
    // int width = getStringWidth( model.getCursorFlagText( aCursor.getIndex(),
    // timestamp ) ) + 10;
    // repaint( cursorPos - 1, 0, width, visibleHeight );
    repaint( 50L ); // XXX
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorChanged( final String aPropertyName, final Cursor aOldCursor, final Cursor aNewCursor )
  {
    // final int visibleHeight = getVisibleRect().height;
    //
    // final TimeLineViewModel model = getModel();
    //
    // int cursorPos = model.timestampToCoordinate( aOldCursor.getTimestamp() );
    // int width = getStringWidth( model.getCursorFlagText(
    // aOldCursor.getIndex(), aOldCursor.getTimestamp() ) );
    // repaint( new Rectangle( cursorPos - ( width / 2 ) - 5, 0, width + 10,
    // visibleHeight ) );
    //
    // cursorPos = model.timestampToCoordinate( aNewCursor.getTimestamp() );
    // width = getStringWidth( model.getCursorFlagText( aNewCursor.getIndex(),
    // aNewCursor.getTimestamp() ) );
    // repaint( 0, cursorPos - ( width / 2 ) - 5, 0, width + 10, visibleHeight
    // );
    repaint( 50L ); // XXX
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cursorRemoved( final Cursor aOldCursor )
  {
    // final int visibleHeight = getVisibleRect().height;
    //
    // final TimeLineViewModel model = getModel();
    //
    // int cursorPos = model.timestampToCoordinate( aOldCursor.getTimestamp() );
    // int width = getStringWidth( model.getCursorFlagText(
    // aOldCursor.getIndex(), aOldCursor.getTimestamp() ) ) + 10;
    // repaint( cursorPos - 1, 0, width, visibleHeight );
    repaint( 50L ); // XXX
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
  public void dataModelChanged( final AcquisitionResult aDataModel )
  {
    repaint( 50L );
  }

  /**
   * @return
   */
  public TimeLineViewModel getModel()
  {
    return this.model;
  }

  /**
   * @return
   */
  public int getTimeLineHeight()
  {
    return getModel().getTimeLineHeight();
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
