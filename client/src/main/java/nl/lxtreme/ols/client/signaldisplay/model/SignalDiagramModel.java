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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.signaldisplay.model;


import java.awt.*;
import java.beans.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.Cursor;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.laf.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.signaldisplay.signalelement.SignalElementManager.SignalElementMeasurer;


/**
 * The main model for the {@link SignalDiagramComponent}.
 */
public class SignalDiagramModel
{
  // INNER TYPES

  /**
   * Denotes where to draw the signal, at the top, center or bottom of the
   * channel.
   */
  public static enum SignalAlignment
  {
    TOP, CENTER, BOTTOM;
  }

  // CONSTANTS

  private static final int SNAP_CURSOR_MODE = ( 1 << 0 );
  private static final int MEASUREMENT_MODE = ( 1 << 1 );

  private static final double TIMESTAMP_FACTOR = 100.0;

  // VARIABLES

  private volatile int mode;
  private volatile int selectedChannelIndex;
  private volatile DataSet dataSet;

  private final ZoomController zoomController;
  private final SignalElementManager channelGroupManager;
  private final SignalDiagramController controller;
  private final EventListenerList eventListeners;
  private final PropertyChangeSupport propertyChangeSupport;

  // CONSTRUCTORS

  /**
   * Creates a new SignalDiagramModel instance.
   *
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   */
  public SignalDiagramModel( final SignalDiagramController aController )
  {
    this.controller = aController;

    this.zoomController = new ZoomController( aController );

    this.channelGroupManager = new SignalElementManager();

    this.eventListeners = new EventListenerList();
    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.mode = 0;

    addDataModelChangeListener( this.channelGroupManager );
  }

  // METHODS

  /**
   * Provides a binary search for arrays of long-values.
   * <p>
   * This implementation is directly copied from the JDK
   * {@link Arrays#binarySearch(long[], long)} implementation, slightly modified
   * to only perform a single comparison-action.
   * </p>
   *
   * @param aArray
   *          the array of long values to search in;
   * @param aFromIndex
   *          the from index to search from;
   * @param aToIndex
   *          the to index to search up and until;
   * @param aKey
   *          the value to search for.
   * @return the index of the given key, which is either the greatest index of
   *         the value less or equal to the given key.
   * @see Arrays#binarySearch(long[], long)
   */
  static final int binarySearch( final long[] aArray, final int aFromIndex, final int aToIndex, final long aKey )
  {
    int mid = -1;
    int low = aFromIndex;
    int high = aToIndex - 1;

    while ( low <= high )
    {
      mid = ( low + high ) >>> 1;
  final long midVal = aArray[mid];

  final int c = ( aKey < midVal ? -1 : ( aKey == midVal ? 0 : 1 ) );
  if ( c > 0 )
  {
    low = mid + 1;
  }
  else if ( c < 0 )
  {
    high = mid - 1;
  }
  else
  {
    return mid; // key found
  }
    }

    if ( mid < 0 )
    {
      return low;
    }

    // Determine the insertion point, avoid crossing the array boundaries...
    if ( mid < ( aToIndex - 1 ) )
    {
      // If the searched value is greater than the value of the found index,
      // insert it after this value, otherwise before it (= the last return)...
      if ( aKey > aArray[mid] )
      {
        return mid + 1;
      }
    }

    return mid;
  }

  /**
   * Moves an element from a "old" position to a "new" position, shifting all
   * other elements.
   * <p>
   * NOTE: the given array's contents will be mutated!
   * </p>
   *
   * @param aInput
   *          the input array to move the elements from, cannot be
   *          <code>null</code>;
   * @param aOldIdx
   *          the index of the element to move;
   * @param aNewIdx
   *          the index to move the element to.
   */
  static final void shiftElements( final int[] aInput, final int aOldIdx, final int aNewIdx )
  {
    final int length = aInput.length;

    final int moved = aInput[aOldIdx];
    // Delete element from array...
    System.arraycopy( aInput, aOldIdx + 1, aInput, aOldIdx, length - 1 - aOldIdx );
    // Make space for new element...
    System.arraycopy( aInput, aNewIdx, aInput, aNewIdx + 1, length - 1 - aNewIdx );
    // Set actual (inserted) element...
    aInput[aNewIdx] = moved;
  }

  /**
   * Adds a cursor change listener.
   *
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addCursorChangeListener( final ICursorChangeListener aListener )
  {
    this.eventListeners.add( ICursorChangeListener.class, aListener );
  }

  /**
   * Adds a data model change listener.
   *
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addDataModelChangeListener( final IDataModelChangeListener aListener )
  {
    this.eventListeners.add( IDataModelChangeListener.class, aListener );
  }

  /**
   * Adds a measurement listener.
   *
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addMeasurementListener( final IMeasurementListener aListener )
  {
    this.eventListeners.add( IMeasurementListener.class, aListener );
  }

  /**
   * Adds a property change listener.
   *
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.addPropertyChangeListener( aListener );
  }

  /**
   * @param aChannelIdx
   * @param aTimestamp
   * @return
   */
  public final long findEdgeAfter( final int aChannelIdx, final long aTimestamp )
  {
    final long[] timestamps = getTimestamps();
    final int[] values = getValues();

    int refIdx = Arrays.binarySearch( timestamps, aTimestamp );
    if ( refIdx < 0 )
    {
      refIdx = -( refIdx + 1 ) - 1;
    }

    if ( ( refIdx < 0 ) || ( refIdx >= values.length ) )
    {
      return timestamps[0];
    }

    // find the reference time value; which is the "timestamp" under the
    // cursor...
    final int mask = ( 1 << aChannelIdx );
    final int refValue = ( values[refIdx] & mask );

    do
    {
      refIdx++;
    }
    while ( ( refIdx < ( values.length - 1 ) ) && ( ( values[refIdx] & mask ) == refValue ) );

    return timestamps[refIdx];
  }

  /**
   * @param aChannelIdx
   * @param aTimestamp
   * @return
   */
  public final long findEdgeBefore( final int aChannelIdx, final long aTimestamp )
  {
    final long[] timestamps = getTimestamps();
    final int[] values = getValues();

    int refIdx = Arrays.binarySearch( timestamps, aTimestamp );
    if ( refIdx < 0 )
    {
      refIdx = -( refIdx + 1 ) - 1;
    }

    if ( ( refIdx < 0 ) || ( refIdx >= values.length ) )
    {
      return timestamps[0];
    }

    // find the reference time value; which is the "timestamp" under the
    // cursor...
    final int mask = ( 1 << aChannelIdx );
    final int refValue = ( values[refIdx] & mask );

    do
    {
      refIdx--;
    }
    while ( ( refIdx > 0 ) && ( ( values[refIdx] & mask ) == refValue ) );

    return timestamps[Math.max( 0, refIdx )];
  }

  /**
   * Finds a UI-element based on a given screen coordinate.
   *
   * @param aPoint
   *          the coordinate to find the channel for, cannot be
   *          <code>null</code>.
   * @return the channel if found, or <code>null</code> if no channel was found.
   */
  public IUIElement findUIElement( final Point aPoint )
  {
    final IUIElement[] elements = getSignalElementManager().getUIElements( aPoint.y, 1,
        SignalElementMeasurer.LOOSE_MEASURER );
    if ( elements.length == 0 )
    {
      return null;
    }
    return elements[0];
  }

  /**
   * @param aPoint
   */
  public void fireMeasurementEvent( final MeasurementInfo aMeasurementInfo )
  {
    final IMeasurementListener[] listeners = this.eventListeners.getListeners( IMeasurementListener.class );
    for ( IMeasurementListener listener : listeners )
    {
      if ( listener.isListening() )
      {
        listener.handleMeasureEvent( aMeasurementInfo );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  public long getAbsoluteLength()
  {
    if ( !hasData() )
    {
      return 0L;
    }

    final AcquisitionResult capturedData = getCapturedData();
    return capturedData.getAbsoluteLength();
  }

  /**
   * @return
   */
  public AcquisitionResult getCapturedData()
  {
    if ( this.dataSet == null )
    {
      return null;
    }
    return this.dataSet.getCapturedData();
  }

  /**
   * {@inheritDoc}
   */
  public Cursor getCursor( final int aCursorIdx )
  {
    final Cursor[] cursors = this.dataSet.getCursors();
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > cursors.length ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    return cursors[aCursorIdx];
  }

  /**
   * Returns all defined cursors.
   *
   * @return an array of defined cursors, never <code>null</code>.
   */
  public Cursor[] getDefinedCursors()
  {
    List<Cursor> result = new ArrayList<Cursor>();

    if ( hasData() )
    {
      for ( Cursor c : this.dataSet.getCursors() )
      {
        if ( c.isDefined() )
        {
          result.add( c );
        }
      }
    }

    return result.toArray( new Cursor[result.size()] );
  }

  /**
   * Returns the time interval displayed by the current view.
   *
   * @return a time interval, in seconds.
   */
  public Double getDisplayedTimeInterval()
  {
    final int width = this.controller.getViewComponent().getVisibleRect().width;
    if ( !hasData() )
    {
      return null;
    }
    double result;
    if ( hasTimingData() )
    {
      result = width / ( getZoomFactor() * getSampleRate() );
    }
    else
    {
      result = width / getZoomFactor();
    }
    return Double.valueOf( result );
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
  public int getHorizontalBlockIncrement( final Rectangle aVisibleRect, final int aDirection )
  {
    final int blockIncr = 50;

    final int firstVisibleSample = locationToSampleIndex( aVisibleRect.getLocation() );
    final int lastVisibleSample = locationToSampleIndex( new Point( aVisibleRect.x + aVisibleRect.width, 0 ) );
    final int lastSampleIdx = getSampleCount();

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
   * @return the minimum height of the signal diagram, in pixels, > 0.
   */
  public int getMinimumHeight()
  {
    return getSignalElementManager().calculateScreenHeight();
  }

  /**
   * {@inheritDoc}
   */
  public int getSampleRate()
  {
    final AcquisitionResult capturedData = getCapturedData();
    if ( capturedData == null )
    {
      return -1;
    }
    return getCapturedData().getSampleRate();
  }

  /**
   * {@inheritDoc}
   */
  public int getSampleWidth()
  {
    final AcquisitionResult capturedData = getCapturedData();
    if ( capturedData == null )
    {
      return 0;
    }
    return capturedData.getChannels();
  }

  /**
   * Returns the index of the current selected channel.
   *
   * @return the selected channel index, or -1 if no channel is selected.
   */
  public int getSelectedChannelIndex()
  {
    return this.selectedChannelIndex;
  }

  /**
   * Returns channel group manager.
   *
   * @return the channel group manager, never <code>null</code>.
   */
  public final SignalElementManager getSignalElementManager()
  {
    return this.channelGroupManager;
  }

  /**
   * Returns the hover area of the signal under the given coordinate (= mouse
   * position).
   *
   * @param aPoint
   *          the mouse coordinate to determine the signal rectangle for, cannot
   *          be <code>null</code>.
   * @return the rectangle of the signal the given coordinate contains,
   *         <code>null</code> if not found.
   */
  public final MeasurementInfo getSignalHover( final Point aPoint )
  {
    final IUIElement element = findUIElement( aPoint );
    if ( !( element instanceof SignalElement ) || !( ( SignalElement )element ).isDigitalSignal() )
    {
      // Trivial reject: no digital signal, or not above any channel...
      return null;
    }

    return getSignalHover( aPoint, ( SignalElement )element );
  }

  /**
   * @param aPoint
   * @param aSignalElement
   * @return
   */
  public MeasurementInfo getSignalHover( final Point aPoint, final SignalElement aSignalElement )
  {
    if ( !aSignalElement.isDigitalSignal() )
    {
      throw new IllegalArgumentException( "Signal element must represent a digital channel!" );
    }

    final int refIdx = locationToSampleIndex( aPoint );
    // Calculate the "absolute" time based on the mouse position, use a
    // "over sampling" factor to allow intermediary (between two time stamps)
    // time value to be shown...
    final double refTime;
    if ( hasTimingData() )
    {
      refTime = getTimestamp( aPoint );
    }
    else
    {
      refTime = refIdx;
    }

    final Channel channel = aSignalElement.getChannel();
    if ( !channel.isEnabled() )
    {
      // Trivial reject: real channel is invisible...
      return new MeasurementInfo( aSignalElement, refTime );
    }

    final long[] timestamps = getTimestamps();

    long ts = -1L;
    long tm = -1L;
    long te = -1L;
    long th = -1L;

    // find the reference time value; which is the "timestamp" under the
    // cursor...
    final int[] values = getValues();
    if ( ( refIdx >= 0 ) && ( refIdx < values.length ) )
    {
      final int mask = channel.getMask();
      final int refValue = ( values[refIdx] & mask );

      int idx = refIdx;
      do
      {
        idx--;
      }
      while ( ( idx >= 0 ) && ( ( values[idx] & mask ) == refValue ) );

      // convert the found index back to "screen" values...
      final int tm_idx = Math.max( 0, idx + 1 );
      tm = ( tm_idx == 0 ) ? 0 : timestamps[tm_idx];

      // Search for the original value again, to complete the pulse...
      do
      {
        idx--;
      }
      while ( ( idx >= 0 ) && ( ( values[idx] & mask ) != refValue ) );

      // convert the found index back to "screen" values...
      final int ts_idx = Math.max( 0, idx + 1 );
      ts = ( ts_idx == 0 ) ? 0 : timestamps[ts_idx];

      idx = refIdx;
      do
      {
        idx++;
      }
      while ( ( idx < values.length ) && ( ( values[idx] & mask ) == refValue ) );

      // convert the found index back to "screen" values...
      final int te_idx = Math.min( idx, timestamps.length - 1 );
      te = ( te_idx == 0 ) ? 0 : timestamps[te_idx];

      // Determine the width of the "high" part...
      if ( ( values[ts_idx] & mask ) != 0 )
      {
        th = Math.abs( tm - ts );
      }
      else
      {
        th = Math.abs( te - tm );
      }
    }

    MeasurementInfo result;
    if ( hasTimingData() )
    {
      result = new MeasurementInfo( aSignalElement, ts, tm, te, th, refTime, getZoomFactor(), getSampleRate() );
    }
    else
    {
      result = new MeasurementInfo( aSignalElement, ts, tm, te, refTime, getZoomFactor(), getSampleRate() );
    }

    return result;
  }

  /**
   * Returns the amount of pixels that represents one second on the timeline.
   *
   * @return the number of pixels to display for 1 second, > 0.
   */
  public Double getTimelinePixelsPerSecond()
  {
    if ( !hasData() || !hasTimingData() )
    {
      return null;
    }
    return Double.valueOf( getZoomFactor() * getSampleRate() );
  }

  /**
   * Returns the number of seconds that represents one pixel on the timeline.
   *
   * @return the number of seconds per pixel, > 0.
   * @see #getTimelinePixelsPerSecond()
   */
  public Double getTimelineSecondsPerPixel()
  {
    Double pixelsPerSecond = getTimelinePixelsPerSecond();
    if ( pixelsPerSecond == null )
    {
      return null;
    }
    return Double.valueOf( 1.0 / pixelsPerSecond.doubleValue() );
  }

  /**
   * Returns the unit of time the timeline is currently is displaying, in
   * multiples of 10 (for human readability).
   *
   * @return a timeline unit of time, > 0, can only be <code>null</code> if
   *         there is no data.
   */
  public Double getTimelineUnitOfTime()
  {
    final Double p = getTimelineSecondsPerPixel();
    if ( p == null )
    {
      return null;
    }
    return Double.valueOf( Math.pow( 10, Math.ceil( Math.log10( p.doubleValue() ) ) ) );
  }

  /**
   * Converts the X-coordinate of the given {@link Point} to a precise
   * timestamp, useful for display purposes.
   *
   * @param aAbsTimestamp
   *          the timestamp to convert to a relative timestamp, should be >= 0.
   * @return a precise timestamp, as double value.
   * @see DisplayUtils#displayTime(double)
   */
  public double getTimestamp( long aAbsTimestamp )
  {
    // Calculate the "absolute" time based on the mouse position, use a
    // "over sampling" factor to allow intermediary (between two time stamps)
    // time value to be shown...
    final double zoomFactor = getZoomFactor();
    final double scaleFactor = TIMESTAMP_FACTOR * zoomFactor;

    // Take (optional) trigger position into account...
    final Long triggerPos = getTriggerPosition();
    if ( triggerPos != null )
    {
      aAbsTimestamp -= triggerPos.longValue();
    }
    // If no sample rate is available, we use a factor of 1; which doesn't
    // make a difference in the result...
    final int sampleRate = Math.max( 1, getSampleRate() );

    return ( scaleFactor * aAbsTimestamp ) / ( scaleFactor * sampleRate );
  }

  /**
   * Converts the X-coordinate of the given {@link Point} to a precise
   * timestamp, useful for display purposes.
   *
   * @param aPoint
   *          the X,Y-coordinate to convert to a precise timestamp, cannot be
   *          <code>null</code>.
   * @return a precise timestamp, as double value.
   * @see DisplayUtils#displayTime(double)
   */
  public double getTimestamp( final Point aPoint )
  {
    // Calculate the "absolute" time based on the mouse position, use a
    // "over sampling" factor to allow intermediary (between two time stamps)
    // time value to be shown...
    final double zoomFactor = getZoomFactor();
    final double scaleFactor = TIMESTAMP_FACTOR * zoomFactor;

    // Convert mouse position to absolute timestamp...
    double x = aPoint.x / zoomFactor;
    // Take (optional) trigger position into account...
    final Long triggerPos = getTriggerPosition();
    if ( triggerPos != null )
    {
      x -= triggerPos.longValue();
    }
    // If no sample rate is available, we use a factor of 1; which doesn't
    // make a difference in the result...
    if ( !hasTimingData() )
    {
      return ( scaleFactor * x ) / scaleFactor;
    }

    return ( scaleFactor * x ) / ( scaleFactor * getSampleRate() );
  }

  /**
   * {@inheritDoc}
   */
  public int getTimestampIndex( final long aValue )
  {
    final AcquisitionResult capturedData = getCapturedData();
    if ( capturedData == null )
    {
      return 0;
    }
    return capturedData.getSampleIndex( aValue );
  }

  /**
   * {@inheritDoc}
   */
  public long[] getTimestamps()
  {
    final AcquisitionResult capturedData = getCapturedData();
    if ( capturedData == null )
    {
      return new long[0];
    }
    return capturedData.getTimestamps();
  }

  /**
   * Returns the trigger position, if available.
   *
   * @return a trigger position, as timestamp, or <code>null</code> if no
   *         trigger is used/present.
   */
  public Long getTriggerPosition()
  {
    AcquisitionResult capturedData = getCapturedData();
    if ( ( capturedData == null ) || !capturedData.hasTriggerData() )
    {
      return null;
    }
    return Long.valueOf( capturedData.getTriggerPosition() );
  }

  /**
   * {@inheritDoc}
   */
  public int[] getValues()
  {
    final AcquisitionResult capturedData = getCapturedData();
    if ( capturedData == null )
    {
      return new int[0];
    }
    return capturedData.getValues();
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
    final SignalElementMeasurer measurer = SignalElementMeasurer.LOOSE_MEASURER;
    final SignalElementManager elemMgr = getSignalElementManager();

    IUIElement[] signalElements = elemMgr.getUIElements( aVisibleRect.y + 1, 1, measurer );
    if ( signalElements.length == 0 )
    {
      return 0;
    }

    final int spacing = UIManager.getInt( UIManagerKeys.SIGNAL_ELEMENT_SPACING );

    int inc = 0;
    int yPos = signalElements[0].getYposition();

    if ( aDirection > 0 )
    {
      // Scroll down...
      int height = signalElements[0].getHeight() + spacing;
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
        signalElements = elemMgr.getUIElements( yPos - spacing, 1, measurer );
        if ( signalElements.length > 0 )
        {
          inc += signalElements[0].getHeight() + spacing;
        }
      }
    }

    return inc;
  }

  /**
   * Returns the zoom controller of this diagram.
   *
   * @return the zoom controller, never <code>null</code>.
   */
  public final ZoomController getZoomController()
  {
    return this.zoomController;
  }

  /**
   * Returns the current zoom factor.
   *
   * @return a zoom factor.
   */
  public final double getZoomFactor()
  {
    return getZoomController().getFactor();
  }

  /**
   * Returns whether or not there is captured data to display.
   *
   * @return <code>true</code> if there is any data to display,
   *         <code>false</code> otherwise.
   */
  public final boolean hasData()
  {
    return ( this.dataSet != null ) && ( getCapturedData() != null );
  }

  /**
   * Returns whether the data is a timed-capture or a state-capture.
   *
   * @return <code>true</code> if there is timing data available,
   *         <code>false</code> if not.
   */
  public boolean hasTimingData()
  {
    AcquisitionResult captureData = getCapturedData();
    return ( captureData != null ) && captureData.hasTimingData();
  }

  /**
   * @return <code>true</code> if the analog scope is by default visible,
   *         <code>false</code> if it is default hidden.
   */
  public boolean isAnalogScopeDefaultVisible()
  {
    return UIManager.getBoolean( UIManagerKeys.ANALOG_SCOPE_VISIBLE_DEFAULT );
  }

  /**
   * {@inheritDoc}
   */
  public boolean isCursorDefined( final int aCursorIdx )
  {
    Cursor[] cursors = this.dataSet.getCursors();
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > cursors.length ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }
    return cursors[aCursorIdx].isDefined();
  }

  /**
   * Returns whether or not the cursor-mode is enabled.
   *
   * @return <code>true</code> if cursor-mode is enabled, thereby making all
   *         defined cursors visible, <code>false</code> otherwise.
   */
  public boolean isCursorMode()
  {
    return ( this.dataSet != null ) && this.dataSet.isCursorsEnabled();
  }

  /**
   * @return <code>true</code> if the group summary is by default visible,
   *         <code>false</code> if it is default hidden.
   */
  public boolean isGroupSummaryDefaultVisible()
  {
    return UIManager.getBoolean( UIManagerKeys.GROUP_SUMMARY_VISIBLE_DEFAULT );
  }

  /**
   * @return
   */
  public boolean isMeasurementMode()
  {
    return ( this.mode & MEASUREMENT_MODE ) != 0;
  }

  /**
   * @return <code>true</code> if the snap cursor mode is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isSnapCursorMode()
  {
    return ( this.mode & SNAP_CURSOR_MODE ) != 0;
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
  public int locationToSampleIndex( final Point aCoordinate )
  {
    final long timestamp = locationToTimestamp( aCoordinate );
    final int idx = getTimestampIndex( timestamp );
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
  public long locationToTimestamp( final Point aCoordinate )
  {
    final long timestamp = ( long )Math.ceil( aCoordinate.x / getZoomFactor() );
    if ( timestamp < 0 )
    {
      return -1;
    }
    return timestamp;
  }

  /**
   * @param aCursorIdx
   */
  public void removeCursor( final int aCursorIdx )
  {
    Cursor[] cursors = this.dataSet.getCursors();
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > cursors.length ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    final Cursor cursor = cursors[aCursorIdx];
    if ( !cursor.isDefined() )
    {
      // Nothing to do; the cursor is not defined...
      return;
    }

    final Cursor oldCursor = cursor.clone();

    cursor.clear();

    ICursorChangeListener[] listeners = this.eventListeners.getListeners( ICursorChangeListener.class );
    for ( ICursorChangeListener listener : listeners )
    {
      listener.cursorRemoved( oldCursor );
    }
  }

  /**
   * Removes a cursor change listener.
   *
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeCursorChangeListener( final ICursorChangeListener aListener )
  {
    this.eventListeners.remove( ICursorChangeListener.class, aListener );
  }

  /**
   * Removes a data model change listener.
   *
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeDataModelChangeListener( final IDataModelChangeListener aListener )
  {
    this.eventListeners.remove( IDataModelChangeListener.class, aListener );
  }

  /**
   * Removes the given measurement listener from the list of listeners.
   *
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeMeasurementListener( final IMeasurementListener aListener )
  {
    this.eventListeners.remove( IMeasurementListener.class, aListener );
  }

  /**
   * Removes a property change listener.
   *
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener )
  {
    this.propertyChangeSupport.removePropertyChangeListener( aListener );
  }

  /**
   * {@inheritDoc}
   */
  public void setCursor( final int aCursorIdx, final long aTimestamp )
  {
    Cursor[] cursors = this.dataSet.getCursors();
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > cursors.length ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    final Cursor cursor = cursors[aCursorIdx];
    final Cursor oldCursor = cursor.clone();

    // Update the time stamp of the cursor...
    cursor.setTimestamp( aTimestamp );

    fireCursorChangeEvent( ICursorChangeListener.PROPERTY_TIMESTAMP, oldCursor, cursor );
  }

  /**
   * Returns the color for a cursor with the given index.
   *
   * @param aCursorIdx
   *          the index of the cursor to retrieve the color for;
   * @param aColor
   *          the color to set, cannot be <code>null</code>.
   * @return a cursor color, never <code>null</code>.
   */
  public void setCursorColor( final int aCursorIdx, final Color aColor )
  {
    Cursor[] cursors = this.dataSet.getCursors();
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > cursors.length ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    final Cursor cursor = cursors[aCursorIdx];
    final Cursor oldCursor = cursor.clone();

    // Update the color of the cursor...
    cursor.setColor( aColor );

    fireCursorChangeEvent( ICursorChangeListener.PROPERTY_COLOR, oldCursor, cursor );
  }

  /**
   * Returns the color for a cursor with the given index.
   *
   * @param aCursorIdx
   *          the index of the cursor to retrieve the color for;
   * @param aLabel
   *          the label to set, cannot be <code>null</code>.
   * @return a cursor color, never <code>null</code>.
   */
  public void setCursorLabel( final int aCursorIdx, final String aLabel )
  {
    Cursor[] cursors = this.dataSet.getCursors();
    if ( ( aCursorIdx < 0 ) || ( aCursorIdx > cursors.length ) )
    {
      throw new IllegalArgumentException( "Invalid cursor index!" );
    }

    final Cursor cursor = cursors[aCursorIdx];
    final Cursor oldCursor = cursor.clone();

    // Update the label of the cursor...
    cursor.setLabel( aLabel );

    fireCursorChangeEvent( ICursorChangeListener.PROPERTY_LABEL, oldCursor, cursor );
  }

  /**
   * Enables or disables the cursors.
   *
   * @param aSelected
   *          <code>true</code> to enable the cursors, <code>false</code> to
   *          disable the cursors.
   */
  public void setCursorMode( final boolean aCursorMode )
  {
    this.dataSet.setCursorsEnabled( aCursorMode );

    ICursorChangeListener[] listeners = this.eventListeners.getListeners( ICursorChangeListener.class );
    for ( ICursorChangeListener listener : listeners )
    {
      if ( aCursorMode )
      {
        listener.cursorsVisible();
      }
      else
      {
        listener.cursorsInvisible();
      }
    }
  }

  /**
   * Sets the data model for this controller.
   *
   * @param aDataSet
   *          the dataModel to set, cannot be <code>null</code>.
   */
  public void setDataModel( final DataSet aDataSet )
  {
    if ( aDataSet == null )
    {
      throw new IllegalArgumentException( "Parameter DataSet cannot be null!" );
    }

    this.dataSet = aDataSet;

    final IDataModelChangeListener[] listeners = this.eventListeners.getListeners( IDataModelChangeListener.class );
    for ( IDataModelChangeListener listener : listeners )
    {
      listener.dataModelChanged( aDataSet );
    }
  }

  /**
   * @param aEnabled
   */
  public void setMeasurementMode( final boolean aEnabled )
  {
    if ( aEnabled )
    {
      this.mode |= MEASUREMENT_MODE;
    }
    else
    {
      this.mode &= ~MEASUREMENT_MODE;
    }

    IMeasurementListener[] listeners = this.eventListeners.getListeners( IMeasurementListener.class );
    for ( IMeasurementListener listener : listeners )
    {
      if ( aEnabled )
      {
        listener.enableMeasurementMode();
      }
      else
      {
        listener.disableMeasurementMode();
      }
    }
  }

  /**
   * Sets the selected channel index to the given value.
   *
   * @param aChannelIndex
   *          the index to set, or -1 if no channel is to be selected.
   */
  public void setSelectedChannelIndex( final int aChannelIndex )
  {
    this.selectedChannelIndex = aChannelIndex;
  }

  /**
   * @param aSnapCursors
   *          the snapCursor to set
   */
  public void setSnapCursorMode( final boolean aSnapCursors )
  {
    if ( aSnapCursors )
    {
      this.mode |= SNAP_CURSOR_MODE;
    }
    else
    {
      this.mode &= ~SNAP_CURSOR_MODE;
    }
  }

  /**
   * @param aOldCursor
   * @param aCursor
   */
  private void fireCursorChangeEvent( final String aPropertyName, final Cursor aOldCursor, final Cursor aCursor )
  {
    ICursorChangeListener[] listeners = this.eventListeners.getListeners( ICursorChangeListener.class );
    for ( ICursorChangeListener listener : listeners )
    {
      if ( !aOldCursor.isDefined() )
      {
        listener.cursorAdded( aCursor );
      }
      else
      {
        listener.cursorChanged( aPropertyName, aOldCursor, aCursor );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  private int getSampleCount()
  {
    return getValues().length;
  }
}
