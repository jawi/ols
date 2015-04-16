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
package nl.lxtreme.ols.client.signaldisplay.model;


import static nl.lxtreme.ols.client.signaldisplay.laf.UIManagerKeys.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElementManager.SignalElementMeasurer;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.client.signaldisplay.util.CursorFlagTextFormatter.LabelStyle;
import nl.lxtreme.ols.util.*;


/**
 * Provides a common base class for the view models.
 */
public abstract class AbstractViewModel
{
  // VARIABLES

  protected final SignalDiagramController controller;
  private final CursorFlagTextFormatter cursorFlagRender;

  // CONSTRUCTORS

  /**
   * Creates a new AbstractViewModel instance.
   *
   * @param aController
   *          the diagram controller to use, cannot be <code>null</code>.
   */
  protected AbstractViewModel( final SignalDiagramController aController )
  {
    this.controller = aController;

    this.cursorFlagRender = new CursorFlagTextFormatter( this.controller.getViewModel() );
  }

  // METHODS

  /**
   * Returns the color for a cursor with the given index.
   *
   * @param aCursorIndex
   *          the index of the cursor to retrieve the color for.
   * @return a cursor color, never <code>null</code>.
   */
  public Color getCursorColor( final int aCursorIndex )
  {
    final Cursor cursor = getSignalDiagramModel().getCursor( aCursorIndex );
    return cursor.getColor();
  }

  /**
   * Returns the cursor flag text for a cursor with the given index.
   *
   * @param aCursorIndex
   *          the index of the cursor to retrieve the flag text for;
   * @param aStyle
   *          the style of the cursor flag text, cannot be <code>null</code>.
   * @return a cursor flag text, never <code>null</code>.
   */
  public String getCursorFlagText( final int aCursorIndex, final LabelStyle aStyle )
  {
    final Cursor cursor = getSignalDiagramModel().getCursor( aCursorIndex );
    return this.cursorFlagRender.getCursorFlagText( cursor, aStyle );
  }

  /**
   * Returns the X-position of the cursor with the given index, for displaying
   * purposes on screen.
   *
   * @param aCursorIdx
   *          the index of the cursor to retrieve the X-position for, >= 0.
   * @return the screen X-position of the cursor with the given index, or -1 if
   *         the cursor is not defined.
   */
  public int getCursorScreenCoordinate( final int aCursorIndex )
  {
    Cursor cursorTimestamp = getSignalDiagramModel().getCursor( aCursorIndex );
    if ( !cursorTimestamp.isDefined() )
    {
      return -1;
    }
    return timestampToCoordinate( cursorTimestamp.getTimestamp() );
  }

  /**
   * Returns the text color for a cursor with the given index.
   *
   * @param aCursorIndex
   *          the index of the cursor to retrieve the color for.
   * @return a cursor text color, never <code>null</code>.
   */
  public Color getCursorTextColor( final int aCursorIndex )
  {
    return ColorUtils.getContrastColor( getCursorColor( aCursorIndex ) );
  }

  /**
   * Returns the index of the current selected channel.
   *
   * @return the current selected channel index, or -1 if no channel is
   *         selected.
   */
  public int getSelectedChannelIndex()
  {
    return getSignalDiagramModel().getSelectedChannelIndex();
  }

  /**
   * @return
   */
  public final SignalElementManager getSignalElementManager()
  {
    return getSignalDiagramModel().getSignalElementManager();
  }

  /**
   * Returns all channels the given range of all visible channel groups.
   *
   * @param aY
   *          the screen Y-coordinate;
   * @param aHeight
   *          the screen height.
   * @return an array of channels, never <code>null</code>.
   */
  public IUIElement[] getSignalElements( final int aY, final int aHeight )
  {
    // Return all channel elements within the given boundaries, even if they do
    // not completely fit...
    return getSignalElementManager().getUIElements( aY, aHeight, SignalElementMeasurer.LOOSE_MEASURER );
  }

  /**
   * Returns the spacing between two signal elements.
   *
   * @return a signal element spacing, in pixels.
   */
  public int getSignalElementSpacing()
  {
    return UIManager.getInt( SIGNAL_ELEMENT_SPACING );
  }

  /**
   * Returns the color to use for painting a trigger moment.
   *
   * @return the trigger color, never <code>null</code>.
   */
  public Color getTriggerColor()
  {
    Color color = UIManager.getColor( SIGNALVIEW_TRIGGER_COLOR );
    if ( color == null )
    {
      color = Color.WHITE;
    }
    return color;
  }

  /**
   * Returns the trigger position, as relative offset from zero.
   *
   * @return a trigger position, as offset.
   */
  public long getTriggerOffset()
  {
    final Long triggerPosition = getSignalDiagramModel().getTriggerPosition();
    if ( triggerPosition == null )
    {
      return 0L;
    }
    return triggerPosition.longValue();
  }

  /**
   * Returns the current zoom factor that is used to display the signals with.
   *
   * @return a zoom factor, >= 0.0.
   */
  public double getZoomFactor()
  {
    return getSignalDiagramModel().getZoomFactor();
  }

  /**
   * Returns whether or not there is captured data to display.
   *
   * @return <code>true</code> if there is any data to display,
   *         <code>false</code> otherwise.
   */
  public final boolean hasData()
  {
    return getSignalDiagramModel().hasData();
  }

  /**
   * Returns whether or not there is trigger data present.
   *
   * @return <code>true</code> if there is trigger data present,
   *         <code>false</code> otherwise.
   */
  public boolean hasTriggerData()
  {
    return getSignalDiagramModel().getTriggerPosition() != null;
  }

  /**
   * @return
   */
  public boolean isCursorMode()
  {
    return getSignalDiagramModel().isCursorMode();
  }

  /**
   * @return
   */
  public boolean isMeasurementMode()
  {
    return getSignalDiagramModel().isMeasurementMode();
  }

  /**
   * Converts a given time stamp to a screen coordinate.
   *
   * @param aTimestamp
   *          the time stamp to convert, >= 0.
   * @return a screen coordinate, >= 0.
   */
  public int timestampToCoordinate( final long aTimestamp )
  {
    double result = getSignalDiagramModel().getZoomFactor() * aTimestamp;
    if ( result > Integer.MAX_VALUE )
    {
      return Integer.MAX_VALUE;
    }
    return ( int )result;
  }

  /**
   * @return
   */
  protected final SignalDiagramModel getSignalDiagramModel()
  {
    return this.controller.getViewModel();
  }

  /**
   * @param aPoint
   * @return
   */
  protected int locationToSampleIndex( final Point aPoint )
  {
    final SignalDiagramModel model = this.controller.getViewModel();
    return model.locationToSampleIndex( aPoint );
  }
}
