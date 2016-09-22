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

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.util.*;
import nl.lxtreme.ols.tool.base.annotation.*;


/**
 * @author jawi
 */
public class OneWireAnalyserTask implements ToolTask<OneWireDataSet>
{
  // CONSTANTS

  private static final String OW_1_WIRE = "1-Wire";

  private static final Logger LOG = Logger.getLogger( OneWireAnalyserTask.class.getName() );

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final AnnotationListener annotationListener;

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
  public OneWireAnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annotationListener = aAnnotationListener;
    this.owTiming = new OneWireTiming( OneWireBusMode.STANDARD );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public OneWireDataSet call() throws Exception
  {
    final AcquisitionResult data = this.context.getData();
    final int[] values = data.getValues();

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

    final OneWireDataSet decodedData = new OneWireDataSet( sampleIdx, sampleCount, data );

    // Update the channel label and clear any existing annotations on the
    // channel...
    prepareResult( OW_1_WIRE );
    // Decode the actual data...
    decodeData( data, decodedData );

    return decodedData;
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
  private void decodeData( final AcquisitionResult aData, final OneWireDataSet aDataSet )
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
		long ttime = ( long )( fallingEdge + ( this.owTiming.getResetFrameLength() / timingCorrection ) );
		if(slavePresent) {
			//slavePresent means there was a falling edge soon enough after the reset rising edge
			//so now lets look for the rising edge of the slavePresent pulse
			 long slavePresentRisingEdge = findEdge(aData, nextFallingEdge, endOfDecode, Edge.RISING );
			 if(slavePresentRisingEdge>0) {
				long nextSlotFallingEdge = findEdge(aData, slavePresentRisingEdge, endOfDecode, Edge.FALLING );
				if(nextSlotFallingEdge>0) {
					//set time one sample before the falling edge of the next slot, like accounting for the recovery time
					time = nextSlotFallingEdge - 1;
				} else {
					//may occur if capture ends before the begining of the next slot
					time = ttime;
				}
			 } else {
					//may occur if capture ends during slavePresent pulse
					time = ttime;
			 }
		} else {
			// Advance the time until *after* the reset pulse... (in case a better criteria was not matched above, since this method does not account for admisable parmeter variation)
			time = ttime;
		}

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
        }
        else if ( this.owTiming.isOne( diff ) )
        {
          // Bytes are sent LSB first, so decode the byte as well with LSB
          // first...
          byteValue |= 0x80;          
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
		
		//looking for the rising edge of the low pulse
		long tRisingEdge = findEdge(aData, fallingEdge, endOfDecode, Edge.RISING );
		long ttime = ( long )( fallingEdge + ( this.owTiming.getBitFrameLength() / timingCorrection ));
		if(tRisingEdge>0) {
		  //rising edge of the pulse is within the capture
		  // now looking for the start of the next slot
		  long tFallingEdge = findEdge(aData, tRisingEdge, endOfDecode, Edge.FALLING );
		  if(tFallingEdge > 0) {
			//start of the next slot is within the capture
			// ending current slot one sample before so that next slot can be identified
			// on the next loop
			time = tFallingEdge - 1;
			if(time>ttime) {
				//when the next slot starts after idle time
				// the current slot limit is considered the maximum slot length
				time = ttime;
			}
		  } else {
			//the next slot does not start within the capture
			// the current slot limit is considered the maximum slot length 
			time = ttime;
		  }
		} else {
		  //the current low pulse does not finish within the capture
		  //  the current slot limit is considered the maximum slot length
		  time = ttime;
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
  private long findEdge( final AcquisitionResult aData, final long aStartOfDecode, final long aEndOfDecode,
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
  private int getDataValue( final AcquisitionResult aData, final long aTimeValue )
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
    this.annotationListener.clearAnnotations( this.owLineIndex );
    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.owLineIndex, aLabel ) );
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
    final AcquisitionResult data = this.context.getData();

    final int startSampleIdx = Math.max( data.getSampleIndex( aStartTimestamp ), 0 );
    aDataSet.reportBusError( this.owLineIndex, startSampleIdx );

    this.annotationListener.onAnnotation( new SampleDataAnnotation( this.owLineIndex, aStartTimestamp, aStartTimestamp,
        OneWireDataSet.OW_BUS_ERROR ) );
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
    final AcquisitionResult data = this.context.getData();
    final int startSampleIdx = Math.max( data.getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( data.getSampleIndex( aEndTimestamp ) - 1, data.getTimestamps().length - 1 );

    aDataSet.reportData( this.owLineIndex, startSampleIdx, endSampleIdx, aByteValue );

    final String annotation = String.format( "0x%X (%c)", Integer.valueOf( aByteValue ), Integer.valueOf( aByteValue ) );
    this.annotationListener.onAnnotation( new SampleDataAnnotation( this.owLineIndex, aStartTimestamp, aEndTimestamp,
        annotation ) );
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
    final AcquisitionResult data = this.context.getData();
    final int startSampleIdx = Math.max( data.getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( data.getSampleIndex( aEndTimestamp ) - 1, data.getTimestamps().length - 1 );

    aDataSet.reportReset( this.owLineIndex, startSampleIdx, endSampleIdx, aSlaveIsPresent );

    final String annotation = String.format( "Master reset, slave %s present", aSlaveIsPresent ? "is" : "is NOT" );
    this.annotationListener.onAnnotation( new SampleDataAnnotation( this.owLineIndex, aStartTimestamp, aEndTimestamp,
        annotation ) );
  }
}
