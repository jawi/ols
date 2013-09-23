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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.i2s;


import static nl.lxtreme.ols.tool.base.NumberUtils.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.base.annotation.*;


/**
 * Performs the actual I2C analysis.
 */
public class I2SAnalyserTask implements ToolTask<I2SDataSet>
{
  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final AnnotationListener annotationListener;

  private int clockIdx;
  private int dataIdx;
  private int wsIdx;

  // CONSTRUCTORS

  /**
   * Creates a new I2CAnalyserTask instance.
   * 
   * @param aContext
   * @param aProgressListener
   */
  public I2SAnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annotationListener = aAnnotationListener;
  }

  // METHODS

  /**
   * This is the I2C protocol decoder core The decoder scans for a decode start
   * event when one of the two lines is going low (start condition). After this
   * the decoder starts to decode the data.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public I2SDataSet call() throws Exception
  {
    final AcquisitionResult data = this.context.getData();

    int startOfDecode = this.context.getStartSampleIndex();
    int endOfDecode = this.context.getEndSampleIndex();

    final int clockMask = ( 1 << this.clockIdx );
    final int dataMask = ( 1 << this.dataIdx );
    final int wsMask = ( 1 << this.wsIdx );

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    final I2SDataSet dataSet = new I2SDataSet( startOfDecode, endOfDecode, data );

    // Prepare everything for the decoding results...
    prepareResults();

    startOfDecode = findFirstWordSelect( data );
    if ( startOfDecode >= endOfDecode )
    {
      throw new IllegalStateException( "Failed to find a word-select edge!" );
    }

    int clockVal;
    int oldClockVal = values[startOfDecode] & clockMask;
    int wsVal;
    int oldWsVal = values[startOfDecode] & wsMask;
    long wordValue = 0;
    int clockCount = 0;
    int startIdx = startOfDecode;

    for ( int i = startOfDecode + 1; i < endOfDecode; i++ )
    {
      clockVal = values[i] & clockMask;
      wsVal = values[i] & wsMask;

      Edge wsEdge = Edge.toEdge( oldWsVal, wsVal );
      Edge clockEdge = Edge.toEdge( oldClockVal, clockVal );

      if ( wsEdge != Edge.NONE )
      {
        reportData( dataSet, timestamps, ( oldWsVal == 0 ) ? Channel.LEFT : Channel.RIGHT, wordValue, startIdx, i );

        clockCount = 0;
        wordValue = 0;
      }

      // Data should be clocked by the receiver on the leading/rising edge of
      // the clock...
      if ( clockEdge.isRising() )
      {
        // the actual data starts one clock *after* WS has changed...
        if ( clockCount == 1 )
        {
          startIdx = i;
        }

        int dataVal = values[i] & dataMask;
        if ( dataVal != 0 )
        {
          wordValue |= 1;
        }
        wordValue <<= 1;
        clockCount++;
      }

      this.progressListener.setProgress( getPercentage( i, startOfDecode, endOfDecode ) );

      oldClockVal = clockVal;
      oldWsVal = wsVal;
    }

    if ( clockCount > 0 )
    {
      reportData( dataSet, timestamps, ( oldWsVal == 0 ) ? Channel.LEFT : Channel.RIGHT, wordValue, startIdx,
          endOfDecode );
    }

    return dataSet;
  }

  /**
   * @param aClockIdx
   */
  public void setClockIndex( final int aClockIdx )
  {
    this.dataIdx = aClockIdx;
  }

  /**
   * @param aDataIdx
   */
  public void setDataIndex( final int aDataIdx )
  {
    this.dataIdx = aDataIdx;
  }

  /**
   * @param aWSIdx
   */
  public void setWordSelectIndex( final int aWSIdx )
  {
    this.wsIdx = aWSIdx;
  }

  /**
   * @return
   */
  private int findFirstWordSelect( final AcquisitionResult aData )
  {
    int idx = this.context.getStartSampleIndex();
    int end = this.context.getEndSampleIndex();

    final int wsMask = ( 1 << this.wsIdx );
    final int[] values = aData.getValues();

    int oldVal = values[idx++] & wsMask;
    while ( idx < end )
    {
      int val = values[idx] & wsMask;
      if ( val != oldVal )
      {
        return idx - 0;
      }
      idx++;
    }

    return idx;
  }

  /**
   * Prepares everything for the upcoming results.
   */
  private void prepareResults()
  {
    // Update the channel labels...
    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.clockIdx, "SCK" ) );
    this.annotationListener.clearAnnotations( this.clockIdx );

    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.dataIdx, "SD" ) );
    this.annotationListener.clearAnnotations( this.dataIdx );

    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.wsIdx, "WS" ) );
    this.annotationListener.clearAnnotations( this.wsIdx );
  }

  /**
   * @param dataSet
   * @param timestamps
   * @param channel
   * @param wordValue
   * @param startIdx
   * @param endIdx
   */
  private void reportData( final I2SDataSet dataSet, final long[] timestamps, Channel channel, long wordValue,
      int startIdx, int endIdx )
  {
    String text = String.format( "%s data: 0x%x", channel.name(), Long.valueOf( wordValue ) );

    dataSet.reportData( this.dataIdx, startIdx, endIdx, channel, wordValue );

    this.annotationListener.onAnnotation( new SampleDataAnnotation( this.dataIdx, timestamps[startIdx],
        timestamps[endIdx], text ) );
  }
}

/* EOF */
