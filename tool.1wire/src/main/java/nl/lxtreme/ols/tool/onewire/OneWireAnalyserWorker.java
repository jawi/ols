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


import static nl.lxtreme.ols.util.NumberUtils.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jawi
 */
public class OneWireAnalyserWorker extends BaseAsyncToolWorker<OneWireDataSet>
{
  // CONSTANTS

  private static final String OW_1_WIRE = "1-Wire";

  private static final Logger LOG = Logger.getLogger( OneWireAnalyserWorker.class.getName() );

  // VARIABLES

  private int owLineIndex;
  private int owLineMask;
  private OneWireTiming owTiming;

  // CONSTRUCTORS

  /**
   * Creates a new OneWireAnalyserWorker instance.
   * 
   * @param aData
   *          the sample data to decode;
   * @param aContext
   *          the tool context to use.
   */
  public OneWireAnalyserWorker( final DataContainer aData, final ToolContext aContext )
  {
    super( aData, aContext );

    this.owTiming = new OneWireTiming( OneWireBusMode.STANDARD );
  }

  // METHODS

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
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected OneWireDataSet doInBackground() throws Exception
  {
    final int[] values = getValues();

    int sampleIdx;

    final int dataMask = this.owLineMask;
    final int sampleCount = values.length;

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, "1-Wire Line mask = 0x{0}", Integer.toHexString( this.owLineMask ) );
    }

    // Search the moment on which the 1-wire line is idle (= high)...
    for ( sampleIdx = 0; sampleIdx < sampleCount; sampleIdx++ )
    {
      final int dataValue = values[sampleIdx];

      if ( ( dataValue & dataMask ) == dataMask )
      {
        // IDLE found here
        break;
      }
    }

    if ( sampleIdx == sampleCount )
    {
      // no idle state could be found
      LOG.log( Level.WARNING, "No IDLE state found in data; aborting analysis..." );
      throw new IllegalStateException( "No IDLE state found!" );
    }

    final OneWireDataSet decodedData = new OneWireDataSet( sampleIdx, sampleCount, this );

    // Update the channel label and clear any existing annotations on the
    // channel...
    prepareResult( OW_1_WIRE );
    // Decode the actual data...
    decodeData( decodedData );

    return decodedData;
  }

  /**
   * Does the actual decoding of the 1-wire data.
   * 
   * @param aDataSet
   *          the decoded data set to add the decoding results to, cannot be
   *          <code>null</code>.
   */
  private void decodeData( final OneWireDataSet aDataSet )
  {
    final long[] timestamps = getTimestamps();

    final long startOfDecode = timestamps[aDataSet.getStartOfDecode()];
    final long endOfDecode = timestamps[aDataSet.getEndOfDecode() - 1];

    // The timing of the 1-wire bus is done in uS, so determine what scale we've
    // to use in order to obtain those kind of time values...
    final double timingCorrection = ( 1.0e6 / getSampleRate() );

    long time = Math.max( 0, startOfDecode );
    setProgress( 0 );

    int bitCount = 8;
    int byteValue = 0;
    long byteStartTime = time;

    while ( ( endOfDecode - time ) > 0 )
    {
      long fallingEdge = findEdge( time, endOfDecode, Edge.FALLING );
      if ( fallingEdge < 0 )
      {
        LOG.log( Level.INFO, "Decoding ended at {0}; no falling edge found...",
            DisplayUtils.displayScaledTime( time, getSampleRate() ) );
        break;
      }
      long risingEdge = findEdge( fallingEdge, endOfDecode, Edge.RISING );
      if ( risingEdge < 0 )
      {
        risingEdge = endOfDecode;
      }

      // Take the difference in time, which should be an indication of what
      // symbol is transmitted...
      final double diff = ( ( risingEdge - fallingEdge ) * timingCorrection );
      if ( this.owTiming.isReset( diff ) )
      {
        // Reset pulse...
        time = ( long )( fallingEdge + ( this.owTiming.getResetFrameLength() / timingCorrection ) );

        // Check for the existence of a "slave present" symbol...
        final boolean slavePresent = isSlavePresent( fallingEdge, time, timingCorrection );
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
          reportBusError( aDataSet, byteStartTime );
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
      setProgress( getPercentage( time, startOfDecode, endOfDecode ) );
    }

    setProgress( 100 );
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
  private long findEdge( final long aStartOfDecode, final long aEndOfDecode, final Edge aEdge )
  {
    long result = -1;

    int oldBitValue = getDataValue( aStartOfDecode ) & this.owLineMask;
    for ( long timeCursor = aStartOfDecode + 1; ( result < 0 ) && ( timeCursor < aEndOfDecode ); timeCursor++ )
    {
      final int bitValue = getDataValue( timeCursor ) & this.owLineMask;

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
  private int getDataValue( final long aTimeValue )
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

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
   * Returns whether between the given timestamps a slave presence pulse was
   * found.
   * <p>
   * A slave presence pulse is defined as: take the difference in time between
   * the first rising and falling edge between the given timestamps, if this
   * difference is beyond a certain threshold this pulse can be considered a
   * slave presence pulse.
   * </p>
   * 
   * @param aStart
   *          the start timestamp;
   * @param aEnd
   *          the end timestamp;
   * @param aMask
   *          the line mask;
   * @param aTimingCorrection
   *          the timing correction to correct the timestamps to microseconds.
   * @return <code>true</code> if a slave presence pulse was found,
   *         <code>false</code> otherwise.
   */
  private boolean isSlavePresent( final long aStart, final long aEnd, final double aTimingCorrection )
  {
    final long risingEdgeTimestamp = findEdge( aStart, aEnd, Edge.RISING );
    if ( risingEdgeTimestamp < 0 )
    {
      return false;
    }

    final long fallingEdgeTimestamp = findEdge( risingEdgeTimestamp, aEnd, Edge.FALLING );
    if ( fallingEdgeTimestamp < 0 )
    {
      return false;
    }

    return this.owTiming.isSlavePresencePulse( ( fallingEdgeTimestamp - risingEdgeTimestamp ) * aTimingCorrection );
  }

  /**
   * Determines the resulting channel label and clears any existing annotations.
   * 
   * @param aLabel
   *          the default label to use for the channel (in case none is set).
   */
  private void prepareResult( final String aLabel )
  {
    updateChannelLabel( this.owLineIndex, aLabel );
    clearChannelAnnotations( this.owLineIndex );
  }

  /**
   * @param aDataSet
   * @param aChannelIndex
   * @param aByteValue
   * @param aType
   * @param aTimestamp
   */
  private void reportBusError( final OneWireDataSet aDataSet, final long aStartTimestamp )
  {
    final int startSampleIdx = Math.max( getSampleIndex( aStartTimestamp ), 0 );

    aDataSet.reportBusError( this.owLineIndex, startSampleIdx );

    addChannelAnnotation( this.owLineIndex, startSampleIdx, startSampleIdx, OneWireDataSet.OW_BUS_ERROR );
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
    final int startSampleIdx = Math.max( getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( getSampleIndex( aEndTimestamp ) - 1, getTimestamps().length - 1 );

    aDataSet.reportData( this.owLineIndex, startSampleIdx, endSampleIdx, aByteValue );

    final String annotation = String.format( "0x%X (%c)", Integer.valueOf( aByteValue ), Integer.valueOf( aByteValue ) );
    addChannelAnnotation( this.owLineIndex, startSampleIdx, endSampleIdx, annotation );
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
    final int startSampleIdx = Math.max( getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( getSampleIndex( aEndTimestamp ) - 1, getTimestamps().length - 1 );

    aDataSet.reportReset( this.owLineIndex, startSampleIdx, endSampleIdx, aSlaveIsPresent );

    final String annotation = String.format( "Master reset, slave %s present", aSlaveIsPresent ? "is" : "is NOT" );
    addChannelAnnotation( this.owLineIndex, startSampleIdx, endSampleIdx, annotation );
  }
}
