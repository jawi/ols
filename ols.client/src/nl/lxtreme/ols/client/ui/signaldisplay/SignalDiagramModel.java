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
package nl.lxtreme.ols.client.ui.signaldisplay;


import static nl.lxtreme.ols.client.ui.signaldisplay.view.UIManagerKeys.*;

import java.awt.*;
import java.beans.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.event.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.signaldisplay.marker.*;
import nl.lxtreme.ols.client.ui.signaldisplay.signalelement.*;
import nl.lxtreme.ols.client.ui.signaldisplay.signalelement.SignalElementManager.SignalElementMeasurer;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.common.session.*;


/**
 * The main model for the {@link SignalDiagramComponent}.
 */
public final class SignalDiagramModel implements PropertyChangeListener
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
  private volatile Marker[] markers;
  private volatile AnnotationHelper annotationsHelper;

  // Injected by Felix DM...
  private volatile Session session;

  private final SignalDiagramController controller;
  private final SignalElementManager channelGroupManager;
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

    this.channelGroupManager = new SignalElementManager();
    this.eventListeners = new EventListenerList();

    this.propertyChangeSupport = new PropertyChangeSupport( this );

    this.mode = 0;

    addDataModelChangeListener( this.channelGroupManager );
  }

  // METHODS

  /**
   * Adds an annotation data change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addAnnotationDataChangedListener( final IAnnotationDataChangedListener aListener )
  {
    this.eventListeners.add( IAnnotationDataChangedListener.class, aListener );
  }

  /**
   * Adds a cursor change listener.
   * 
   * @param aListener
   *          the listener to add, cannot be <code>null</code>.
   */
  public void addCursorChangeListener( final IMarkerChangeListener aListener )
  {
    this.eventListeners.add( IMarkerChangeListener.class, aListener );
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
  public long findEdgeAfter( final SignalElement aSignalElement, final long aTimestamp )
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
    final int mask = aSignalElement.getMask();
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
  public long findEdgeBefore( final SignalElement aSignalElement, final long aTimestamp )
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
    final int mask = aSignalElement.getMask();
    final int refValue = ( values[refIdx] & mask );

    do
    {
      refIdx--;
    }
    while ( ( refIdx > 0 ) && ( ( values[refIdx] & mask ) == refValue ) );

    return timestamps[Math.max( 0, refIdx )];
  }

  /**
   * Finds a signal element based on a given screen coordinate.
   * 
   * @param aPoint
   *          the coordinate to find the channel for, cannot be
   *          <code>null</code>.
   * @return the channel if found, or <code>null</code> if no channel was found.
   */
  public SignalElement findSignalElement( final Point aPoint )
  {
    final SignalElement[] elements = getSignalElementManager().getSignalElements( aPoint.y, 1,
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

    final AcquisitionData capturedData = getAcquisitionData();
    return capturedData.getAbsoluteLength();
  }

  /**
   * @return the acquired data, can be <code>null</code>.
   */
  public AcquisitionData getAcquisitionData()
  {
    return this.session.getAcquisitionData();
  }

  /**
   * @return the annotation data, never <code>null</code>.
   */
  public AnnotationData getAnnotationData()
  {
    return this.session.getAnnotationData();
  }

  /**
   * Returns the current value of annotationsHelper.
   * 
   * @return the annotation helper, never <code>null</code>.
   */
  public AnnotationHelper getAnnotationHelper()
  {
    return this.annotationsHelper;
  }

  /**
   * Returns all defined markers, that is, all markers that have a defined time
   * stamp.
   * 
   * @return an array of defined markers, never <code>null</code>.
   */
  public Marker[] getDefinedMarkers()
  {
    List<Marker> result = new ArrayList<Marker>();
    if ( this.markers != null )
    {
      for ( Marker marker : this.markers )
      {
        if ( marker.isDefined() )
        {
          result.add( marker );
        }
      }
    }
    return result.toArray( new Marker[result.size()] );
  }

  /**
   * Returns the time interval displayed by the current view.
   * 
   * @return a time interval, in seconds.
   */
  public Double getDisplayedTimeInterval()
  {
    final Rectangle visibleRect = this.controller.getSignalDiagram().getOuterViewSize();
    if ( ( visibleRect == null ) || !hasData() )
    {
      return null;
    }
    double result;
    if ( hasTimingData() )
    {
      result = visibleRect.width / ( getZoomFactor() * getSampleRate() );
    }
    else
    {
      result = visibleRect.width / getZoomFactor();
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
   * Returns the maximum width of the screen.
   * 
   * @return a screen width, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int getMaximalScreenWidth()
  {
    final double result = Math.floor( getAbsoluteLength() * getZoomFactor() );
    if ( result > Integer.MAX_VALUE )
    {
      return Integer.MAX_VALUE;
    }
    return ( int )result;
  }

  /**
   * Returns the minimum height of the screen.
   * 
   * @return a screen height, in pixels, >= 0 && < {@value Integer#MAX_VALUE}.
   */
  public int getMinimalScreenHeight()
  {
    return getSignalElementManager().calculateScreenHeight();
  }

  /**
   * Returns all movable markers.
   * 
   * @return an array with moveable markers, never <code>null</code>.
   */
  public Marker[] getMoveableMarkers()
  {
    List<Marker> result = new ArrayList<Marker>();
    if ( this.markers != null )
    {
      for ( Marker marker : this.markers )
      {
        if ( marker.isMoveable() )
        {
          result.add( marker );
        }
      }
    }
    return result.toArray( new Marker[result.size()] );
  }

  /**
   * {@inheritDoc}
   */
  public int getSampleRate()
  {
    final AcquisitionData capturedData = getAcquisitionData();
    if ( capturedData == null )
    {
      return -1;
    }
    return getAcquisitionData().getSampleRate();
  }

  /**
   * {@inheritDoc}
   */
  public int getSampleWidth()
  {
    final AcquisitionData capturedData = getAcquisitionData();
    if ( capturedData == null )
    {
      return 0;
    }
    return capturedData.getChannelCount();
  }

  /**
   * Returns the current selected channel.
   * 
   * @return the selected channel, or <code>null</code> if no channel is
   *         selected.
   */
  public SignalElement getSelectedChannel()
  {
    return getSignalElementManager().getChannelByIndex( this.selectedChannelIndex );
  }

  /**
   * Returns the signal element representing the digital channel with the given
   * index.
   * 
   * @param aIndex
   *          the index of the signal element to return, >= 0.
   * @return a signal element, can be <code>null</code>.
   */
  public SignalElement getSignalElement( final int aIndex )
  {
    return getSignalElementManager().getChannelByIndex( aIndex );
  }

  /**
   * Returns channel group manager.
   * 
   * @return the channel group manager, never <code>null</code>.
   */
  public SignalElementManager getSignalElementManager()
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
  public MeasurementInfo getSignalHover( final Point aPoint )
  {
    final SignalElement signalElement = findSignalElement( aPoint );
    if ( ( signalElement == null ) || !signalElement.isDigitalSignal() )
    {
      // Trivial reject: no digital signal, or not above any channel...
      return null;
    }

    return getSignalHover( aPoint, signalElement );
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
    return getTimestamp( ( long )( aPoint.x / getZoomFactor() ) );
  }

  /**
   * {@inheritDoc}
   */
  public int getTimestampIndex( final long aValue )
  {
    final AcquisitionData capturedData = getAcquisitionData();
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
    final AcquisitionData capturedData = getAcquisitionData();
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
    AcquisitionData capturedData = getAcquisitionData();
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
    final AcquisitionData capturedData = getAcquisitionData();
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

    SignalElement[] signalElements = elemMgr.getSignalElements( aVisibleRect.y + 1, 1, measurer );
    if ( signalElements.length == 0 )
    {
      return 0;
    }

    final int spacing = UIManager.getInt( SIGNAL_ELEMENT_SPACING );

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
        signalElements = elemMgr.getSignalElements( yPos - spacing, 1, measurer );
        if ( signalElements.length > 0 )
        {
          inc += signalElements[0].getHeight() + spacing;
        }
      }
    }

    return inc;
  }

  /**
   * Returns the current zoom factor.
   * 
   * @return a zoom factor.
   */
  public double getZoomFactor()
  {
    return this.controller.getZoomController().getFactor();
  }

  /**
   * Returns whether or not there is captured data to display.
   * 
   * @return <code>true</code> if there is any data to display,
   *         <code>false</code> otherwise.
   */
  public boolean hasData()
  {
    return ( getAcquisitionData() != null );
  }

  /**
   * Returns whether the data is a timed-capture or a state-capture.
   * 
   * @return <code>true</code> if there is timing data available,
   *         <code>false</code> if not.
   */
  public boolean hasTimingData()
  {
    AcquisitionData captureData = getAcquisitionData();
    return ( captureData != null ) && captureData.hasTimingData();
  }

  /**
   * @return <code>true</code> if the analog scope is by default visible,
   *         <code>false</code> if it is default hidden.
   */
  public boolean isAnalogScopeDefaultVisible()
  {
    return UIManager.getBoolean( ANALOG_SCOPE_VISIBLE_DEFAULT );
  }

  /**
   * Returns whether or not the cursor-mode is enabled.
   * 
   * @return <code>true</code> if cursor-mode is enabled, thereby making all
   *         defined cursors visible, <code>false</code> otherwise.
   */
  public boolean isCursorMode()
  {
    CursorController controller = Client.getInstance().getCursorController();
    return controller.isCursorsVisible();
  }

  /**
   * @return <code>true</code> if the group summary is by default visible,
   *         <code>false</code> if it is default hidden.
   */
  public boolean isGroupSummaryDefaultVisible()
  {
    return UIManager.getBoolean( GROUP_SUMMARY_VISIBLE_DEFAULT );
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
   * Converts the given {@link PropertyChangeEvent} to a cursor event.
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvent )
  {
    if ( aEvent.getSource() instanceof Marker )
    {
      Marker marker = ( Marker )aEvent.getSource();

      if ( ( aEvent.getOldValue() == null ) && ( aEvent.getNewValue() != null ) )
      {
        // Marker was added...
        fireMarkerAddedEvent( marker );
      }
      else if ( ( aEvent.getOldValue() != null ) && ( aEvent.getNewValue() == null ) )
      {
        // Marker was removed...
        fireMarkerRemovedEvent( marker );
      }
      else
      {
        // Property changed...
        String name = aEvent.getPropertyName();
        if ( "timestamp".equals( name ) )
        {
          long oldValue = ( ( Long )aEvent.getOldValue() ).longValue();
          long newValue = ( ( Long )aEvent.getNewValue() ).longValue();
          fireMarkerMoveEvent( marker, oldValue, newValue );
        }
        else
        {
          fireMarkerChangeEvent( name, marker );
        }
      }
    }
  }

  /**
   * Removes an annotation data change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeAnnotationDataChangedListener( final IAnnotationDataChangedListener aListener )
  {
    this.eventListeners.remove( IAnnotationDataChangedListener.class, aListener );
  }

  /**
   * Removes a cursor change listener.
   * 
   * @param aListener
   *          the listener to remove, cannot be <code>null</code>.
   */
  public void removeCursorChangeListener( final IMarkerChangeListener aListener )
  {
    this.eventListeners.remove( IMarkerChangeListener.class, aListener );
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
   * Sets the data model for this controller.
   * 
   * @param aDataSet
   *          the dataModel to set, cannot be <code>null</code>.
   */
  public void setAcquisitionData( final AcquisitionData aData )
  {
    if ( aData == null )
    {
      throw new IllegalArgumentException( "Data cannot be null!" );
    }

    this.annotationsHelper = new AnnotationHelper( this.session );

    createMarkers( aData );

    fireDataModelChangedEvent();
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
    CursorController controller = Client.getInstance().getCursorController();
    controller.setCursorsVisible( aCursorMode );

    IMarkerChangeListener[] listeners = this.eventListeners.getListeners( IMarkerChangeListener.class );
    for ( IMarkerChangeListener listener : listeners )
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
   * Fires an event that the annotation data has been changed.
   */
  void fireAnnotationDataChangedEvent()
  {
    IAnnotationDataChangedListener[] listeners = this.eventListeners
        .getListeners( IAnnotationDataChangedListener.class );
    for ( IAnnotationDataChangedListener listener : listeners )
    {
      listener.annotationDataChanged( getAnnotationData() );
    }
  }

  /**
   * Notifies listeners that either all annotations or annotations of a single
   * channel are cleared.
   * 
   * @param aChannelIdx
   *          the channel index of the channel whose annotations are cleared, or
   *          <code>null</code> if all annotations are cleared.
   */
  final void fireAnnotationDataClearedEvent( final Integer aChannelIdx )
  {
    IAnnotationDataChangedListener[] listeners = this.eventListeners
        .getListeners( IAnnotationDataChangedListener.class );
    for ( IAnnotationDataChangedListener listener : listeners )
    {
      listener.annotationDataCleared( aChannelIdx );
    }
  }

  /**
   * Called by Felix DM upon initialization of this component.
   */
  void init( final org.apache.felix.dm.Component aComponent )
  {
    // Session is injected by now; create our annotation helper...
    this.annotationsHelper = new AnnotationHelper( this.session );

    // Restore the acquisition data of the session...
    AcquisitionData data = this.session.getAcquisitionData();
    if ( data != null )
    {
      setAcquisitionData( data );
    }
  }

  /**
   * @param aData
   */
  private void createMarkers( final AcquisitionData aData )
  {
    if ( this.markers != null )
    {
      for ( Marker marker : this.markers )
      {
        marker.removePropertyChangeListener( this );
      }
    }

    this.markers = new Marker[Ols.MAX_CURSORS + 1];
    this.markers[0] = new Marker( aData.getTriggerPosition() );

    int i = 1;
    for ( Cursor cursor : aData.getCursors() )
    {
      final Marker marker = new Marker( cursor );
      // Register ourselves as property change listeners for this marker...
      marker.addPropertyChangeListener( this );

      this.markers[i++] = marker;
    }
  }

  /**
   * Fires an event to all {@link IDataModelChangeListener}s that the data model
   * is changed.
   */
  private void fireDataModelChangedEvent()
  {
    final IDataModelChangeListener[] listeners = this.eventListeners.getListeners( IDataModelChangeListener.class );
    for ( IDataModelChangeListener listener : listeners )
    {
      listener.dataModelChanged( getAcquisitionData() );
    }
  }

  /**
   * @param aOldCursor
   * @param aCursor
   */
  private void fireMarkerAddedEvent( final Marker aCursor )
  {
    IMarkerChangeListener[] listeners = this.eventListeners.getListeners( IMarkerChangeListener.class );
    for ( IMarkerChangeListener listener : listeners )
    {
      listener.markerAdded( aCursor );
    }
  }

  /**
   * @param aOldCursor
   * @param aCursor
   */
  private void fireMarkerChangeEvent( final String aPropertyName, final Marker aCursor )
  {
    IMarkerChangeListener[] listeners = this.eventListeners.getListeners( IMarkerChangeListener.class );
    for ( IMarkerChangeListener listener : listeners )
    {
      listener.markerChanged( aPropertyName, aCursor );
    }
  }

  /**
   * @param aOldCursor
   * @param aCursor
   */
  private void fireMarkerMoveEvent( final Marker aCursor, final long aOldTimestamp, final long aNewTimestamp )
  {
    IMarkerChangeListener[] listeners = this.eventListeners.getListeners( IMarkerChangeListener.class );
    for ( IMarkerChangeListener listener : listeners )
    {
      listener.markerMoved( aOldTimestamp, aNewTimestamp );
    }
  }

  /**
   * @param aCursor
   */
  private void fireMarkerRemovedEvent( final Marker aCursor )
  {
    IMarkerChangeListener[] listeners = this.eventListeners.getListeners( IMarkerChangeListener.class );
    for ( IMarkerChangeListener listener : listeners )
    {
      listener.markerRemoved( aCursor );
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
