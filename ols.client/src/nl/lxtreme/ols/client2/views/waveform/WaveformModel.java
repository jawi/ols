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
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.MeasurementInfoBuilder.MeasurementInfo;
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
   * Returns the absolute width of the screen.
   * 
   * @return a screen width, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int calculateScreenWidth()
  {
    return ( int )( getZoomFactor() * getAbsoluteLength() );
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
   * Finds the cursor timestamp using a given mouse location and taking the
   * "snap cursor" mode into consideration.
   * 
   * @param aPoint
   *          the coordinate of the potential cursor, cannot be
   *          <code>null</code>.
   * @return a timestamp, or -1 if no valid timestamp could be determined.
   */
  public long findCursorTimestamp( Point aPoint )
  {
    long timestamp = coordinateToTimestamp( aPoint );
    if ( !isCursorSnapMode() )
    {
      return timestamp;
    }

    AcquisitionData data = getData();
    WaveformElement element = getSelectedElement();
    int refIdx = data.getSampleIndex( timestamp );

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    // find the reference time value; which is the "timestamp" under the
    // cursor...
    if ( ( refIdx >= 0 ) && ( refIdx < values.length ) )
    {
      int mask = element.getMask();
      int refValue = ( values[refIdx] & mask );

      int idx = refIdx;
      do
      {
        idx--;
      }
      while ( ( idx >= 0 ) && ( ( values[idx] & mask ) == refValue ) );

      // convert the found index back to "screen" values...
      final int tm_idx = Math.max( 0, idx + 1 );
      return timestamps[tm_idx];
    }

    return timestamp;
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
   * @return the radix to display the values in a group summary.
   */
  public Radix getGroupSummaryRadix()
  {
    return Radix.HEX; // XXX
  }

  /**
   * Calculates the horizontal block increment.
   * <p>
   * The following rules are adhered for scrolling horizontally:
   * </p>
   * <ol>
   * <li>unless the first or last sample is not shown, scroll a full block;
   * otherwise</li>
   * <li>do not scroll.</li>
   * </ol>
   * 
   * @param aVisibleRect
   *          the visible rectangle of the component, never <code>null</code>;
   * @param aDirection
   *          the direction in which to scroll (&gt; 0 to scroll left, &lt; 0 to
   *          scroll right);
   * @return a horizontal block increment, determined according to the rules
   *         described.
   */
  public int getHorizontalBlockIncrement( Rectangle aVisibleRect, int aDirection )
  {
    int blockIncr = 50;

    int firstVisibleSample = coordinateToSampleIndex( aVisibleRect.getLocation() );
    int lastVisibleSample = coordinateToSampleIndex( new Point( aVisibleRect.x + aVisibleRect.width, 0 ) );
    int lastSampleIdx = getSampleCount();

    int inc = 0;
    if ( aDirection < 0 )
    {
      // Scroll left
      if ( firstVisibleSample >= 0 )
      {
        inc = blockIncr;
      }
    }
    else if ( aDirection > 0 )
    {
      // Scroll right
      if ( lastVisibleSample < lastSampleIdx )
      {
        inc = blockIncr;
      }
    }

    return inc;
  }

  /**
   * Determines the measurement information for the given mouse position.
   * 
   * @param aPoint
   *          the mouse position to retrieve the measurement information for,
   *          cannot be <code>null</code>;
   * @param aElement
   *          the waveform element to get the measurement information for,
   *          cannot be <code>null</code>.
   * @return the measurement information, can be <code>null</code>.
   */
  public MeasurementInfo getMeasurementInfo( Point aPoint, WaveformElement aElement )
  {
    AcquisitionData data = getData();

    int sampleRate = data.getSampleRate();
    double zoomFactor = getZoomFactor();
    // Calculate the "absolute" time based on the mouse position, use a
    // "over sampling" factor to allow intermediary (between two time stamps)
    // time value to be shown...
    double scaleFactor = 100.0 * zoomFactor;

    // Convert mouse position to absolute timestamp...
    double x = aPoint.x / zoomFactor;

    long timestamp = ( long )Math.ceil( x );
    int refIdx = data.getSampleIndex( timestamp );

    // If no sample rate is available, we use a factor of 1; which doesn't
    // make a difference in the result...
    double refTime;
    if ( !data.hasTimingData() )
    {
      refTime = ( scaleFactor * x ) / scaleFactor;
    }
    else
    {
      // Take (optional) trigger position into account...
      if ( data.hasTriggerData() )
      {
        x -= data.getTriggerPosition();
      }

      refTime = ( scaleFactor * x ) / ( scaleFactor * sampleRate );
    }

    MeasurementInfoBuilder infoBuilder = new MeasurementInfoBuilder( data, zoomFactor );
    infoBuilder.setReferenceTime( refTime );
    infoBuilder.setChannel( aElement.getChannel() );
    infoBuilder.setYposition( aElement.getYposition() + aElement.getOffset() );
    infoBuilder.setHeight( aElement.getSignalHeight() );

    if ( !aElement.isEnabled() )
    {
      return infoBuilder.build();
    }

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    // find the reference time value; which is the "timestamp" under the
    // cursor...
    if ( ( refIdx >= 0 ) && ( refIdx < values.length ) )
    {
      int mask = aElement.getMask();
      int refValue = ( values[refIdx] & mask );

      int idx = refIdx;
      do
      {
        idx--;
      }
      while ( ( idx >= 0 ) && ( ( values[idx] & mask ) == refValue ) );

      // convert the found index back to "screen" values...
      final int tm_idx = Math.max( 0, idx + 1 );
      long tm = ( tm_idx == 0 ) ? 0 : timestamps[tm_idx];

      infoBuilder.setTransitionTime( tm );

      // Search for the original value again, to complete the pulse...
      do
      {
        idx--;
      }
      while ( ( idx >= 0 ) && ( ( values[idx] & mask ) != refValue ) );

      // convert the found index back to "screen" values...
      final int ts_idx = Math.max( 0, idx + 1 );
      long ts = ( ts_idx == 0 ) ? 0 : timestamps[ts_idx];

      infoBuilder.setStartTime( ts );

      idx = refIdx;
      do
      {
        idx++;
      }
      while ( ( idx < values.length ) && ( ( values[idx] & mask ) == refValue ) );

      // convert the found index back to "screen" values...
      final int te_idx = Math.min( idx, timestamps.length - 1 );
      long te = ( te_idx == 0 ) ? 0 : timestamps[te_idx];

      infoBuilder.setEndTime( te );

      // Determine the width of the "high" part...
      if ( ( values[ts_idx] & mask ) != 0 )
      {
        infoBuilder.setHighTime( Math.abs( tm - ts ) );
      }
      else
      {
        infoBuilder.setHighTime( Math.abs( te - tm ) );
      }
    }

    return infoBuilder.build();
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
   * Calculates the vertical block increment.
   * <p>
   * The following rules are adhered for scrolling vertically:
   * </p>
   * <ol>
   * <li>if the first shown channel is not completely visible, it will be made
   * fully visible; otherwise</li>
   * <li>scroll down to show the succeeding channel fully;</li>
   * <li>if the last channel is fully shown, and there is some room left at the
   * bottom, show the remaining space.</li>
   * </ol>
   * 
   * @param aVisibleRect
   *          the visible rectangle of the component, never <code>null</code>;
   * @param aDirection
   *          the direction in which to scroll (&gt; 0 to scroll down, &lt; 0 to
   *          scroll up).
   * @return a vertical block increment, determined according to the rules
   *         described.
   */
  public int getVerticalBlockIncrement( final Dimension aViewDimensions, final Rectangle aVisibleRect,
      final int aDirection )
  {
    WaveformElement[] elements = getWaveformElements( aVisibleRect.y + 1, 1, LOOSE_MEASURER );
    if ( elements.length == 0 )
    {
      return 0;
    }

    final int spacing = UIManager.getInt( SIGNAL_ELEMENT_SPACING );

    int inc = 0;
    int yPos = elements[0].getYposition();

    if ( aDirection > 0 )
    {
      // Scroll down...
      int height = elements[0].getHeight() + spacing;
      inc = height - ( aVisibleRect.y - yPos );
      if ( inc < 0 )
      {
        inc = -inc;
      }
    }
    else if ( aDirection < 0 )
    {
      // Scroll up...
      inc = ( aVisibleRect.y - yPos );
      if ( inc <= 0 )
      {
        // Determine the height of the element *before* the current one, as we
        // need to scroll up its height...
        elements = getWaveformElements( yPos - spacing, 1, LOOSE_MEASURER );
        if ( elements.length > 0 )
        {
          inc += elements[0].getHeight() + spacing;
        }
      }
    }

    return inc;
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
   * @return <code>true</code> if cursors snap mode is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isCursorSnapMode()
  {
    ManagedAction action = this.controller.getActionManager().getAction( SetCursorSnapModeAction.ID );
    return ( action == null ) ? false : Boolean.TRUE.equals( action.getValue( Action.SELECTED_KEY ) );
  }

  /**
   * @return <code>true</code> if measurement mode is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isMeasurementMode()
  {
    ManagedAction action = this.controller.getActionManager().getAction( SetMeasurementModeAction.ID );
    return ( action == null ) ? false : Boolean.TRUE.equals( action.getValue( Action.SELECTED_KEY ) );
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
