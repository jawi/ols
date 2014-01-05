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


import static nl.lxtreme.ols.common.annotation.DataAnnotation.*;
import static nl.lxtreme.ols.tool.base.NumberUtils.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Performs the actual I2C analysis.
 */
public class I2SAnalyserTask implements ToolTask<I2SDataSet>
{
  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final ToolAnnotationHelper annHelper;

  private int startOfDecode;
  private int endOfDecode;

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
  public I2SAnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annHelper = new ToolAnnotationHelper( aContext );
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
    final AcquisitionData data = this.context.getData();

    final int clockMask = ( 1 << this.clockIdx );
    final int dataMask = ( 1 << this.dataIdx );
    final int wsMask = ( 1 << this.wsIdx );

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    final I2SDataSet dataSet = new I2SDataSet( this.startOfDecode, this.endOfDecode, data );

    // Prepare everything for the decoding results...
    prepareResults();

    this.startOfDecode = findFirstWordSelect( data );
    if ( this.startOfDecode >= this.endOfDecode )
    {
      throw new IllegalStateException( "Failed to find a word-select edge!" );
    }

    int clockVal;
    int oldClockVal = values[this.startOfDecode] & clockMask;
    int wsVal;
    int oldWsVal = values[this.startOfDecode] & wsMask;
    long wordValue = 0;
    int clockCount = 0;
    int startIdx = this.startOfDecode;

    for ( int i = this.startOfDecode + 1; i < this.endOfDecode; i++ )
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

      this.progressListener.setProgress( getPercentage( i, this.startOfDecode, this.endOfDecode ) );

      oldClockVal = clockVal;
      oldWsVal = wsVal;
    }

    if ( clockCount > 0 )
    {
      reportData( dataSet, timestamps, ( oldWsVal == 0 ) ? Channel.LEFT : Channel.RIGHT, wordValue, startIdx,
          this.endOfDecode );
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
   * Sets the decoding area.
   * 
   * @param aStartOfDecode
   *          a start sample index, >= 0;
   * @param aEndOfDecode
   *          a ending sample index, >= 0.
   */
  public void setDecodingArea( final int aStartOfDecode, final int aEndOfDecode )
  {
    this.startOfDecode = aStartOfDecode;
    this.endOfDecode = aEndOfDecode;
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
  private int findFirstWordSelect( final AcquisitionData aData )
  {
    int idx = this.startOfDecode;
    int end = this.endOfDecode;

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
    this.annHelper.clearAnnotations( this.clockIdx, this.dataIdx, this.wsIdx );

    this.annHelper.addLabelAnnotation( this.clockIdx, "SCK" );
    this.annHelper.addLabelAnnotation( this.dataIdx, "SD" );
    this.annHelper.addLabelAnnotation( this.wsIdx, "WS" );
  }

  /**
   * @param dataSet
   * @param timestamps
   * @param channel
   * @param wordValue
   * @param startIdx
   * @param endIdx
   */
  private void reportData( final I2SDataSet dataSet, final long[] timestamps, final Channel channel,
      final long wordValue, final int startIdx, final int endIdx )
  {
    String color = Channel.LEFT == channel ? "#cccccc" : "#eeeeee";
    String text = String.format( "%s data: 0x%x", channel.name(), Long.valueOf( wordValue ) );

    dataSet.reportData( this.dataIdx, startIdx, endIdx, channel, wordValue );

    this.annHelper.addAnnotation( this.dataIdx, timestamps[startIdx], timestamps[endIdx], Long.valueOf( wordValue ),
        KEY_COLOR, color, KEY_DESCRIPTION, text );
  }
}

/* EOF */
