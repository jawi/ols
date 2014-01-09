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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.waveform;


import static nl.lxtreme.ols.client2.views.UIMgr.*;
import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.*;
import static nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer.*;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.Client.JumpDirection;
import nl.lxtreme.ols.client2.Client.JumpType;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.Type;
import nl.lxtreme.ols.client2.views.waveform.WaveformElement.WaveformElementMeasurer;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;


/**
 * Provides a custom model for the {@link WaveformView}.
 */
public class WaveformModel extends ViewModel
{
  // INNER TYPES

  /**
   * Provides a {@link Cursor}-comparator on timestamps (instead of comparing on
   * indices).
   */
  private static class CursorComparator implements Comparator<Cursor>
  {
    @Override
    public int compare( Cursor aCursor1, Cursor aCursor2 )
    {
      return ( int )( aCursor1.getTimestamp() - aCursor2.getTimestamp() );
    }
  }

  // CONSTANTS

  /**
   * Defines the area around each cursor in which the mouse cursor should be in
   * before the cursor can be moved.
   */
  private static final int CURSOR_SENSITIVITY_AREA = 4;

  // VARIABLES

  private final ViewController controller;
  private final List<WaveformElement> elements;
  private final AtomicReference<WaveformElement> selectedElementRef;

  // CONSTRUCTORS

  /**
   * Creates a new {@link WaveformModel} instance.
   * 
   * @param aSession
   *          the session to use for this model, cannot be <code>null</code>.
   */
  public WaveformModel( ViewController aController, Session aSession )
  {
    super( aSession );

    this.controller = aController;

    this.elements = new CopyOnWriteArrayList<WaveformElement>();
    this.selectedElementRef = new AtomicReference<WaveformElement>();

    AcquisitionData data = aSession.getAcquiredData();
    for ( ChannelGroup group : data.getChannelGroups() )
    {
      this.elements.add( createGroupElement( group ) );

      for ( Channel channel : group.getChannels() )
      {
        this.elements.add( createChannelElement( channel ) );
      }

      this.elements.add( createGroupSummary( group ) );
      this.elements.add( createAnalogScope( group ) );
    }
  }

  // METHODS

  /**
   * Returns the absolute height of the screen.
   * 
   * @param aHeightProvider
   *          the provider for the various element's heights, cannot be
   *          <code>null</code>.
   * @return a screen height, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int calculateScreenHeight()
  {
    int height = 0;

    final int spacing = UIManager.getInt( SIGNAL_ELEMENT_SPACING );
    for ( WaveformElement element : this.elements )
    {
      height += element.getHeight() + spacing;
    }

    return height;
  }

  /**
   * Converts the given coordinate to the corresponding sample index.
   * 
   * @param aCoordinate
   *          the coordinate to convert to a sample index, cannot be
   *          <code>null</code>.
   * @return a sample index, >= 0, or -1 if no corresponding sample index could
   *         be found.
   */
  public int coordinateToSampleIndex( final Point aCoordinate )
  {
    final long timestamp = coordinateToTimestamp( aCoordinate );
    final int idx = getData().getSampleIndex( timestamp );
    if ( idx < 0 )
    {
      return -1;
    }
    final int sampleCount = getSampleCount() - 1;
    if ( idx > sampleCount )
    {
      return sampleCount;
    }

    return idx;
  }

  /**
   * Converts the given coordinate to the corresponding sample index.
   * 
   * @param aCoordinate
   *          the coordinate to convert to a sample index, cannot be
   *          <code>null</code>.
   * @return a sample index, >= 0, or -1 if no corresponding sample index could
   *         be found.
   */
  public long coordinateToTimestamp( final Point aCoordinate )
  {
    final long timestamp = ( long )Math.ceil( aCoordinate.x / getZoomFactor() );
    if ( timestamp < 0 )
    {
      return -1;
    }
    return timestamp;
  }

  /**
   * Finds the cursor under the given point.
   * 
   * @param aPoint
   *          the coordinate of the potential cursor, cannot be
   *          <code>null</code>.
   * @return the cursor index, or -1 if not found.
   */
  public Cursor findCursor( Point aPoint )
  {
    final long refIdx = coordinateToTimestamp( aPoint );
    final double snapArea = CURSOR_SENSITIVITY_AREA / getZoomFactor();

    for ( Cursor cursor : getData().getCursors() )
    {
      if ( cursor.inArea( refIdx, snapArea ) )
      {
        return cursor;
      }
    }

    return null;
  }

  /**
   * Finds the waveform element based on a given screen coordinate.
   * 
   * @param aPoint
   *          the coordinate to find the channel for, cannot be
   *          <code>null</code>.
   * @return the channel if found, or <code>null</code> if no channel was found.
   */
  public WaveformElement findWaveformElement( Point aPoint )
  {
    final WaveformElement[] elements = getWaveformElements( aPoint.y, 1, LOOSE_MEASURER );
    if ( elements.length == 0 )
    {
      return null;
    }
    return elements[0];
  }

  /**
   * @see AcquisitionData#getAbsoluteLength()
   */
  public long getAbsoluteLength()
  {
    return getData().getAbsoluteLength();
  }

  /**
   * @param aClip
   * @return
   */
  public int getEndIndex( Rectangle aClip, int aLength )
  {
    final Point location = new Point( aClip.x + aClip.width, 0 );
    int index = coordinateToSampleIndex( location );
    return Math.min( index + 1, aLength - 1 );
  }

  /**
   * @return the number of samples available, >= 0.
   */
  public int getSampleCount()
  {
    return getData().getValues().length;
  }

  /**
   * Returns the selected waveform element (= the element under the mouse
   * cursor).
   * 
   * @return a waveform element, can be <code>null</code>.
   */
  public WaveformElement getSelectedElement()
  {
    return this.selectedElementRef.get();
  }

  /**
   * @param aClip
   * @return
   */
  public int getStartIndex( Rectangle aClip )
  {
    final Point location = aClip.getLocation();
    int index = coordinateToSampleIndex( location );
    return Math.max( index - 1, 0 );
  }

  /**
   * Returns the {@link WaveformElement} corresponding to the given channel
   * index.
   * 
   * @param aChannelIdx
   *          the index of the channel to return the waveform element for, >= 0.
   * @return a {@link WaveformElement} corresponding to the given channel index,
   *         or <code>null</code> if no such element could be found.
   */
  public WaveformElement getWaveformElement( Channel aChannel )
  {
    if ( aChannel == null )
    {
      return null;
    }

    int idx = aChannel.getIndex();
    for ( WaveformElement element : this.elements )
    {
      if ( Type.CHANNEL.equals( element.getType() ) && element.getIndex() == idx )
      {
        return element;
      }
    }
    return null;
  }

  /**
   * @return a collection of all waveform elements, never <code>null</code>.
   */
  public Collection<WaveformElement> getWaveformElements()
  {
    return this.elements;
  }

  /**
   * @param aY
   *          the starting position on the screen;
   * @param aHeight
   *          the height of the screen, in pixels;
   * @param aMeasurer
   *          the element measurer to use, cannot be <code>null</code>.
   * @return the waveform elements that fit in the given area.
   */
  public WaveformElement[] getWaveformElements( int aY, int aHeight, WaveformElementMeasurer aMeasurer )
  {
    final List<WaveformElement> result = new ArrayList<WaveformElement>();

    final int yMin = aY;
    final int yMax = aHeight + aY;

    final int spacing = UIManager.getInt( SIGNAL_ELEMENT_SPACING );
    final int halfSpacing = spacing / 2;

    int yPos = 0;
    for ( WaveformElement element : this.elements )
    {
      if ( yPos > yMax )
      {
        // Optimization: no need to continue after the requested end position...
        break;
      }

      int height = element.getHeight();
      if ( aMeasurer.signalElementFits( yPos, height + halfSpacing, yMin, yMax ) )
      {
        element.setYposition( yPos );
        result.add( element );
      }
      yPos += height + spacing;
    }

    return result.toArray( new WaveformElement[result.size()] );
  }

  /**
   * @return a zoom factor.
   */
  public double getZoomFactor()
  {
    return ( ( WaveformView )this.controller.getView() ).getZoomFactor();
  }

  /**
   * Sets the selected element to the one given.
   * 
   * @param aElement
   *          the selected element, can be <code>null</code>.
   */
  public void setSelectedElement( WaveformElement aElement )
  {
    WaveformElement old;
    do
    {
      old = this.selectedElementRef.get();
    }
    while ( !this.selectedElementRef.compareAndSet( old, aElement ) );
  }

  /**
   * Performs a "smart" jump in a given direction.
   * 
   * @param aType
   *          what kind of jump to perform;
   * @param aReferencePoint
   *          the timestamp to take as reference for the jump;
   * @param aDirection
   *          in what direction to jump.
   */
  public long smartJump( JumpType aType, Point aReferencePoint, JumpDirection aDirection )
  {
    WaveformElement element = getSelectedElement();
    if ( ( element == null ) || !Type.CHANNEL.equals( element.getType() ) )
    {
      return -1L;
    }

    long refTimestamp = coordinateToTimestamp( aReferencePoint );

    switch ( aType )
    {
      case ANNOTATION:
        return determineAnnotationJump( element, refTimestamp, aDirection );

      case CURSOR:
        return determineCursorJump( refTimestamp, aDirection );

      case SIGNAL_EDGE:
        return determineEdgeJump( element, refTimestamp, aDirection );

      default:
        return -1L;
    }
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
    double result = getZoomFactor() * aTimestamp;
    if ( result > Integer.MAX_VALUE )
    {
      return Integer.MAX_VALUE;
    }
    return ( int )result;
  }

  /**
   * Finds the first annotation (strictly) before or after a given reference
   * point in time.
   * 
   * @param aElement
   *          the waveform element representing the signal to search in;
   * @param aTimestamp
   *          the reference point in time to start searching from;
   * @param aDirection
   *          the direction in which to search.
   * @return the timestamp of the found annotation, or -1L in case no such
   *         annotation could be found.
   */
  private long determineAnnotationJump( WaveformElement aElement, long aTimestamp, JumpDirection aDirection )
  {
    AnnotationData annotations = getAnnotations();

    SortedSet<DataAnnotation> result;
    if ( aDirection.isLeft() )
    {
      result = annotations.getAnnotations( aElement.getIndex(), aTimestamp - 2, aTimestamp - 1 );
    }
    else
    {
      result = annotations.getAnnotations( aElement.getIndex(), aTimestamp + 1, aTimestamp + 2 );
    }

    if ( result.size() > 0 )
    {
      DataAnnotation annotation = result.first();

      long start = annotation.getStartTimestamp();
      long end = annotation.getEndTimestamp();

      return start + ( ( end - start ) / 2L );
    }

    return -1L;
  }

  /**
   * Finds the first cursor (strictly) before or after a given reference point
   * in time.
   * 
   * @param aTimestamp
   *          the reference point in time to start searching from;
   * @param aDirection
   *          the direction in which to search.
   * @return the timestamp of the found cursor, or -1L in case no such cursor
   *         could be found or the cursors are not visible.
   */
  private long determineCursorJump( long aTimestamp, JumpDirection aDirection )
  {
    AcquisitionData data = getData();
    if ( !data.areCursorsVisible() )
    {
      return -1L;
    }

    Cursor[] allCursors = data.getCursors();
    SortedSet<Cursor> cursors = new TreeSet<Cursor>( new CursorComparator() );
    for ( Cursor cursor : allCursors )
    {
      if ( cursor.isDefined() )
      {
        cursors.add( cursor );
      }
    }

    Cursor foundCursor = null;
    for ( Cursor cursor : cursors )
    {
      long timestamp = cursor.getTimestamp();
      if ( aDirection.isLeft() )
      {
        if ( timestamp < aTimestamp )
        {
          foundCursor = cursor;
        }
        else
        {
          break;
        }
      }
      else
      {
        if ( timestamp > aTimestamp )
        {
          foundCursor = cursor;
          break;
        }
      }
    }

    if ( foundCursor != null )
    {
      return foundCursor.getTimestamp();
    }

    return -1L;
  }

  /**
   * Finds the first signal transition (strictly) before or after a given
   * reference point in time.
   * 
   * @param aElement
   *          the waveform element representing the signal to search in;
   * @param aTimestamp
   *          the reference point in time to start searching from;
   * @param aDirection
   *          the direction in which to search.
   * @return the timestamp of the found signal transition, or -1L in case no
   *         such signal transition could be found.
   */
  private long determineEdgeJump( WaveformElement aElement, long aTimestamp, JumpDirection aDirection )
  {
    AcquisitionData data = getData();
    long[] timestamps = data.getTimestamps();
    int[] values = data.getValues();

    int refIdx = data.getSampleIndex( aTimestamp );

    // find the reference time value; which is the "timestamp" under the
    // cursor...
    final int mask = aElement.getMask();
    final int refValue = ( values[refIdx] & mask );

    if ( aDirection.isLeft() )
    {
      do
      {
        refIdx--;
      }
      while ( ( refIdx > 0 ) && ( ( values[refIdx] & mask ) == refValue ) );
    }
    else
    {
      do
      {
        refIdx++;
      }
      while ( ( refIdx < ( values.length - 1 ) ) && ( ( values[refIdx] & mask ) == refValue ) );
    }

    return timestamps[refIdx];
  }
}
