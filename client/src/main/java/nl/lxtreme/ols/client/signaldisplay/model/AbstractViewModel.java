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


import java.awt.*;

import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.channel.*;
import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalElementMeasurer;
import nl.lxtreme.ols.util.*;


/**
 * Provides a common base class for the view models.
 */
public abstract class AbstractViewModel
{
  // INNER TYPES

  /**
   * Denotes how to represent a cursor label. Used for automatic placement of
   * cursor labels.
   */
  public static enum LabelStyle
  {
    INDEX_ONLY, TIME_ONLY, LABEL_ONLY, INDEX_LABEL, LABEL_TIME;
  }

  // VARIABLES

  protected final SignalDiagramController controller;

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
  }

  // METHODS

  /**
   * @return
   */
  public final ChannelGroupManager getChannelGroupManager()
  {
    return getSignalDiagramModel().getChannelGroupManager();
  }

  /**
   * @return
   */
  public int getChannelHeight()
  {
    return getSignalDiagramModel().getChannelHeight();
  }

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
    if ( !cursor.isDefined() )
    {
      return "";
    }
    return getCursorFlagText( aCursorIndex, cursor.getTimestamp(), aStyle );
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
    return ( getCursorColor( aCursorIndex ) );
  }

  /**
   * @return
   */
  public int getGroupSummaryHeight()
  {
    return getSignalDiagramModel().getGroupSummaryHeight();
  }

  /**
   * @return
   */
  public int getSampleWidth()
  {
    return getSignalDiagramModel().getSampleWidth();
  }

  /**
   * @return
   */
  public int getScopeHeight()
  {
    return getSignalDiagramModel().getScopeHeight();
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
  public SignalElement[] getSignalElements( final int aY, final int aHeight )
  {
    // Return all channel elements within the given boundaries, even if they do
    // not completely fit...
    return getSignalDiagramModel().getSignalElements( aY, aHeight, SignalElementMeasurer.LOOSE_MEASURER );
  }

  /**
   * Returns the signal group height.
   * 
   * @return the signal group height, in pixels.
   */
  public int getSignalGroupHeight()
  {
    return getSignalDiagramModel().getSignalHeight();
  }

  /**
   * @return
   */
  public int getSignalHeight()
  {
    return getSignalDiagramModel().getSignalHeight();
  }

  /**
   * Returns the signal offset.
   * 
   * @return a signal offset, >= 0.
   */
  public int getSignalOffset()
  {
    return getSignalDiagramModel().getSignalOffset();
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
    return this.controller.getSignalDiagramModel();
  }

  /**
   * @param aPoint
   * @return
   */
  protected int locationToSampleIndex( final Point aPoint )
  {
    final SignalDiagramModel model = this.controller.getSignalDiagram().getModel();
    return model.locationToSampleIndex( aPoint );
  }

  /**
   * @param aPoint
   * @return
   */
  protected long locationToTimestamp( final Point aPoint )
  {
    final SignalDiagramModel model = this.controller.getSignalDiagram().getModel();
    return model.locationToTimestamp( aPoint );
  }

  /**
   * Returns the cursor flag text for the cursor with the given index.
   * 
   * @param aCursorIdx
   *          the index of the cursor, >= 0 && < 10;
   * @param aCursorTimestamp
   *          the timestamp of the cursor;
   * @param aStyle
   *          the style of the cursor flag text, cannot be <code>null</code>.
   * @return a cursor flag text, or an empty string if the cursor with the given
   *         index is undefined.
   */
  private String getCursorFlagText( final int aCursorIdx, final long aCursorTimestamp, final LabelStyle aStyle )
  {
    final SignalDiagramModel model = getSignalDiagramModel();
    final double sampleRate = model.getSampleRate();

    final Cursor cursor = model.getCursor( aCursorIdx );
    Integer index = Integer.valueOf( aCursorIdx + 1 );

    String label = cursor.getLabel();
    if ( !cursor.hasLabel() )
    {
      label = index.toString();
    }

    switch ( aStyle )
    {
      case LABEL_TIME:
        return label.concat( ": " ).concat( DisplayUtils.displayTime( aCursorTimestamp / sampleRate ) );
      case INDEX_LABEL:
        return String.format( "%d: %s", index, label );
      case TIME_ONLY:
        return DisplayUtils.displayTime( aCursorTimestamp / sampleRate );
      case LABEL_ONLY:
        return label;
      case INDEX_ONLY:
      default:
        return String.format( "%d", index );
    }
  }
}
