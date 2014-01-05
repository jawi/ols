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
package nl.lxtreme.ols.tool.onewire;


import static nl.lxtreme.ols.common.annotation.DataAnnotation.*;
import static nl.lxtreme.ols.tool.base.NumberUtils.*;

import java.util.logging.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * @author jawi
 */
public class OneWireAnalyserTask implements ToolTask<OneWireDataSet>
{
  // CONSTANTS

  /** Denotes a bus-reset event. */
  static final String EVENT_RESET = "RESET";
  /** Denotes a bus-error event. */
  static final String EVENT_BUS_ERROR = "BUS-ERROR";
  /**
   * Denotes a property whose (boolean) value indicated whether or not a slave
   * was present.
   */
  static final String KEY_SLAVE_PRESENT = "Slave present";

  private static final String OW_1_WIRE = "1-Wire";

  private static final Logger LOG = Logger.getLogger( OneWireAnalyserTask.class.getName() );

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final ToolAnnotationHelper annHelper;

  private int startOfDecode;
  private int endOfDecode;

  private int owLineIndex;
  private int owLineMask;
  private OneWireTiming owTiming;

  // CONSTRUCTORS

  /**
   * Creates a new OneWireAnalyserWorker instance.
   * 
   * @param aContext
   * @param aProgressListener
   *          the progress listener to use for reporting the progress, cannot be
   *          <code>null</code>.
   */
  public OneWireAnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annHelper = new ToolAnnotationHelper( aContext );
    this.owTiming = new OneWireTiming( OneWireBusMode.STANDARD );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public OneWireDataSet call() throws Exception
  {
    final AcquisitionData data = this.context.getData();
    final int[] values = data.getValues();

    int sampleIdx;

    final int dataMask = this.owLineMask;

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, "1-Wire Line mask = 0x{0}", Integer.toHexString( this.owLineMask ) );
    }

    // Search the moment on which the 1-wire line is idle (= high)...
    for ( sampleIdx = this.startOfDecode; sampleIdx < this.endOfDecode; sampleIdx++ )
    {
      final int dataValue = values[sampleIdx];

      if ( ( dataValue & dataMask ) == dataMask )
      {
        // IDLE found here
        break;
      }
    }

    if ( sampleIdx >= this.endOfDecode )
    {
      // no idle state could be found
      LOG.log( Level.WARNING, "No IDLE state found in data; aborting analysis..." );
      throw new IllegalStateException( "No IDLE state found!" );
    }

    final OneWireDataSet decodedData = new OneWireDataSet( sampleIdx, this.endOfDecode, data );

    // Update the channel label and clear any existing annotations on the
    // channel...
    prepareResult( OW_1_WIRE );
    // Decode the actual data...
    decodeData( data, decodedData );

    return decodedData;
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
   * Sets the 1-wire bus mode.
   * 
   * @param aOneWireBusMode
   *          the bus mode, either {@link OneWireBusMode#STANDARD}, or
   *          {@link OneWireBusMode#OVERDRIVE}, cannot be <code>null</code>.
   */
  public void setOneWireBusMode( final OneWireBusMode aBusMode )
  {
    this.owTiming = new OneWireTiming( aBusMode );
  }

  /**
   * Sets the line index of which the 1-wire data should be decoded from.
   * 
   * @param aIndex
   *          the index of the 1-wire data, >= 0.
   */
  public void setOneWireLineIndex( final int aIndex )
  {
    this.owLineIndex = aIndex;
    this.owLineMask = ( 1 << aIndex );
  }

  /**
   * Does the actual decoding of the 1-wire data.
   * 
   * @param aDataSet
   *          the decoded data set to add the decoding results to, cannot be
   *          <code>null</code>.
   */
  private void decodeData( final AcquisitionData aData, final OneWireDataSet aDataSet )
  {
    final long[] timestamps = aData.getTimestamps();

    this.progressListener.setProgress( 0 );

    final long startOfDecode = timestamps[aDataSet.getStartOfDecode()];
    final long endOfDecode = timestamps[aDataSet.getEndOfDecode() - 1];

    // The timing of the 1-wire bus is done in uS, so determine what scale we've
    // to use in order to obtain those kind of time values...
    final double timingCorrection = ( 1.0e6 / aData.getSampleRate() );

    long time = Math.max( 0, startOfDecode );

    int bitCount = 8;
    int byteValue = 0;
    long byteStartTime = time;

    while ( ( endOfDecode - time ) > 0 )
    {
      long fallingEdge = findEdge( aData, time, endOfDecode, Edge.FALLING );
      if ( fallingEdge < 0 )
      {
        LOG.log( Level.INFO, "Decoding ended at {0}; no falling edge found...",
            Unit.Time.format( time / ( double )aData.getSampleRate() ) );
        break;
      }
      long risingEdge = findEdge( aData, fallingEdge, endOfDecode, Edge.RISING );
      if ( risingEdge < 0 )
      {
        risingEdge = endOfDecode;
      }

      // Take the difference in time, which should be an indication of what
      // symbol is transmitted...
      final double diff = ( ( risingEdge - fallingEdge ) * timingCorrection );
      if ( this.owTiming.isReset( diff ) )
      {
        // Take the next falling edge, whose difference with the last leading
        // edge should indicate the presence of a slave or not...
        final long nextFallingEdge = findEdge( aData, risingEdge, endOfDecode, Edge.FALLING );

        boolean slavePresent = false;
        if ( nextFallingEdge > 0 )
        {
          // Found, lets check whether it is a valid slave presence pulse...
          slavePresent = this.owTiming.isSlavePresencePulse( ( nextFallingEdge - risingEdge ) * timingCorrection );
        }

        // Advance the time until *after* the reset pulse...
        time = ( long )( fallingEdge + ( this.owTiming.getResetFrameLength() / timingCorrection ) );

        LOG.log( Level.FINE, "Master bus reset; slave is {0}present...", ( slavePresent ? "" : "NOT " ) );

        reportReset( aDataSet, fallingEdge, time, slavePresent );
      }
      else
      {
        if ( bitCount == 8 )
        {
          // Take the falling edge of the most significant bit as start of our
          // decoded byte value...
          byteStartTime = fallingEdge;
        }

        if ( this.owTiming.isZero( diff ) )
        {
          // Zero bit: only update timing...
          time = ( long )( fallingEdge + ( this.owTiming.getBitFrameLength() / timingCorrection ) );
        }
        else if ( this.owTiming.isOne( diff ) )
        {
          // Bytes are sent LSB first, so decode the byte as well with LSB
          // first...
          byteValue |= 0x80;
          time = ( long )( fallingEdge + ( this.owTiming.getBitFrameLength() / timingCorrection ) );
        }
        else
        {
          // Unknown symbol; report it as bus error and restart our byte...
          time = ( long )( fallingEdge + ( this.owTiming.getBitFrameLength() / timingCorrection ) );

          reportBusError( aDataSet, byteStartTime, time );

          byteValue = 0;
          bitCount = 8;
          time = fallingEdge;
          // Don't bother continuing; instead start over...
          continue;
        }

        if ( --bitCount == 0 )
        {
          // Report the complete byte value...
          reportData( aDataSet, byteStartTime, time, byteValue );

          byteValue = 0;
          bitCount = 8;
        }
        else
        {
          byteValue >>= 1;
        }
      }

      // Update progress...
      this.progressListener.setProgress( getPercentage( time, startOfDecode, endOfDecode ) );
    }

    this.progressListener.setProgress( 100 );
  }

  /**
   * Find first falling edge this is the start of the start bit. If the signal
   * is inverted, find the first rising edge.
   * 
   * @param aStartOfDecode
   *          the timestamp to start searching;
   * @param aEndOfDecode
   *          the timestamp to end the search;
   * @param aMask
   *          the bit-value mask to apply for finding the start bit.
   * @return the time at which the start bit was found, -1 if it is not found.
   */
  private long findEdge( final AcquisitionData aData, final long aStartOfDecode, final long aEndOfDecode,
      final Edge aEdge )
  {
    long result = -1;

    int oldBitValue = getDataValue( aData, aStartOfDecode ) & this.owLineMask;
    for ( long timeCursor = aStartOfDecode + 1; ( result < 0 ) && ( timeCursor < aEndOfDecode ); timeCursor++ )
    {
      final int bitValue = getDataValue( aData, timeCursor ) & this.owLineMask;

      final Edge edge = Edge.toEdge( oldBitValue, bitValue );
      if ( aEdge == edge )
      {
        result = timeCursor;
      }

      oldBitValue = bitValue;
    }

    return result;
  }

  /**
   * Returns the data value for the given time stamp.
   * 
   * @param aTimeValue
   *          the time stamp to return the data value for.
   * @return the data value of the sample index right before the given time
   *         value.
   */
  private int getDataValue( final AcquisitionData aData, final long aTimeValue )
  {
    final int[] values = aData.getValues();
    final long[] timestamps = aData.getTimestamps();

    int i;
    for ( i = 1; i < timestamps.length; i++ )
    {
      if ( aTimeValue < timestamps[i] )
      {
        break;
      }
    }
    return values[i - 1];
  }

  /**
   * Determines the resulting channel label and clears any existing annotations.
   * 
   * @param aLabel
   *          the default label to use for the channel (in case none is set).
   */
  private void prepareResult( final String aLabel )
  {
    this.annHelper.clearAnnotations( this.owLineIndex );
    this.annHelper.addLabelAnnotation( this.owLineIndex, aLabel );
  }

  /**
   * @param aDataSet
   * @param aChannelIndex
   * @param aByteValue
   * @param aType
   * @param aTimestamp
   */
  private void reportBusError( final OneWireDataSet aDataSet, final long aStartTimestamp, final long aEndTimestamp )
  {
    final AcquisitionData data = this.context.getData();

    final int startSampleIdx = Math.max( data.getSampleIndex( aStartTimestamp ), 0 );
    aDataSet.reportBusError( this.owLineIndex, startSampleIdx );

    this.annHelper.addErrorAnnotation( this.owLineIndex, aStartTimestamp, aEndTimestamp, EVENT_BUS_ERROR, KEY_COLOR,
        "#ff8000", KEY_DESCRIPTION, "Timing issue." );
  }

  /**
   * @param aDataSet
   * @param aChannelIndex
   * @param aByteValue
   * @param aType
   * @param aTimestamp
   */
  private void reportData( final OneWireDataSet aDataSet, final long aStartTimestamp, final long aEndTimestamp,
      final int aByteValue )
  {
    final AcquisitionData data = this.context.getData();
    final int startSampleIdx = Math.max( data.getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( data.getSampleIndex( aEndTimestamp ) - 1, data.getTimestamps().length - 1 );

    aDataSet.reportData( this.owLineIndex, startSampleIdx, endSampleIdx, aByteValue );

    this.annHelper.addSymbolAnnotation( this.owLineIndex, aStartTimestamp, aEndTimestamp, aByteValue );
  }

  /**
   * @param aDataSet
   * @param aChannelIndex
   * @param aByteValue
   * @param aType
   * @param aTimestamp
   */
  private void reportReset( final OneWireDataSet aDataSet, final long aStartTimestamp, final long aEndTimestamp,
      final boolean aSlaveIsPresent )
  {
    final AcquisitionData data = this.context.getData();
    final int startSampleIdx = Math.max( data.getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( data.getSampleIndex( aEndTimestamp ) - 1, data.getTimestamps().length - 1 );

    aDataSet.reportReset( this.owLineIndex, startSampleIdx, endSampleIdx, aSlaveIsPresent );

    String desc = String.format( "Bus reset, slave %s present", aSlaveIsPresent ? "is" : "is NOT" );

    this.annHelper.addEventAnnotation( this.owLineIndex, aStartTimestamp, aEndTimestamp, EVENT_RESET, KEY_COLOR,
        "#e0e0e0", KEY_DESCRIPTION, desc, KEY_SLAVE_PRESENT, Boolean.valueOf( aSlaveIsPresent ) );
  }
}
