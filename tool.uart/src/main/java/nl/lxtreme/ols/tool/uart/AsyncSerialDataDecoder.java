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
package nl.lxtreme.ols.tool.uart;


import static nl.lxtreme.ols.util.NumberUtils.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a generic decoder for asynchronous serial data, such as used for
 * UARTs, smartcards, LIN and other protocols.
 */
public final class AsyncSerialDataDecoder
{
  // INNER TYPES

  /**
   * Denotes the parity, as used in serial protocols for simple data integrity
   * checks.
   */
  public static enum Parity
  {
    // CONSTANTS

    /** No parity check. */
    NONE,
    /** The parity bit forms an odd number of 1-bits. */
    ODD,
    /** The parity bit forms an even number of 1-bits. */
    EVEN;

    // METHODS

    /**
     * Returns whether or not this parity is {@link #EVEN}.
     * 
     * @return <code>true</code> if this parity is even, <code>false</code>
     *         otherwise.
     */
    public boolean isEven()
    {
      return this == EVEN;
    }

    /**
     * Returns whether or not this parity is {@link #NONE}.
     * 
     * @return <code>true</code> if this parity is none, <code>false</code>
     *         otherwise.
     */
    public boolean isNone()
    {
      return this == NONE;
    }

    /**
     * Returns whether or not this parity is {@link #ODD}.
     * 
     * @return <code>true</code> if this parity is odd, <code>false</code>
     *         otherwise.
     */
    public boolean isOdd()
    {
      return this == ODD;
    }
  }

  /**
   * Denotes the configuration used to decode the serial data.
   */
  public static class SerialConfiguration
  {
    // VARIABLES

    private final int dataBits;
    private final int baudRate;
    private final StopBits stopBits;
    private final Parity parity;
    private final boolean inverted;
    private final boolean lsbFirst;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SerialConfiguration} instance, defaulting to 8N1,
     * 9600 baud, no inversion of the signal and most-significant bit first.
     */
    public SerialConfiguration()
    {
      this( 9600, 8, StopBits.ONE, Parity.NONE, false /* inverted */, false /* lsbFirst */);
    }

    /**
     * Creates a new {@link SerialConfiguration} instance using the given data
     * values.
     * 
     * @param aBaudRate
     *          the baudrate, in bps;
     * @param aDataBits
     *          the number of bits in a single data symbol, > 0;
     * @param aStopBits
     *          the number of stop bits;
     * @param aParity
     *          what form of parity is used;
     * @param aInverted
     *          <code>true</code> if the entire signal is inverted,
     *          <code>false</code> if it is normal;
     * @param aLsbFirst
     *          <code>true</code> if the least-significant bit comes first in a
     *          symbol, <code>false</code> if the most-significant bit comes
     *          first in a symbol.
     */
    public SerialConfiguration( final int aBaudRate, final int aDataBits, final StopBits aStopBits,
        final Parity aParity, final boolean aInverted, final boolean aLsbFirst )
    {
      this.baudRate = aBaudRate;
      this.dataBits = aDataBits;
      this.stopBits = aStopBits;
      this.parity = aParity;
      this.inverted = aInverted;
      this.lsbFirst = aLsbFirst;
    }

    // METHODS

    /**
     * Returns the current value of baudRate.
     * 
     * @return the baudRate
     */
    public int getBaudRate()
    {
      return this.baudRate;
    }

    /**
     * Calculates the center for a single bit.
     * 
     * @param aSampleRate
     *          the sample rate to use, in Hertz, > 0.
     * @return a bit center.
     */
    public int getBitLength( final int aSampleRate )
    {
      return aSampleRate / this.baudRate;
    }

    /**
     * Returns the current value of dataBits.
     * 
     * @return the dataBits
     */
    public int getDataBits()
    {
      return this.dataBits;
    }

    /**
     * Calculates the total frame size, that is, the number of data bits plus
     * stop bits and parity bit.
     * 
     * @param aSampleRate
     *          the sample rate to use, in Hertz, > 0.
     * @return a frame size, in bits per second.
     */
    public int getFrameSize( final int aSampleRate )
    {
      final int bitLength = getBitLength( aSampleRate );
      final int stopCount = ( int )Math.ceil( this.stopBits.getValue() );
      final int parityCount = this.parity.isNone() ? 0 : 1;
      return ( ( this.dataBits + stopCount + parityCount ) * bitLength );
    }

    /**
     * Returns the current value of parity.
     * 
     * @return the parity
     */
    public Parity getParity()
    {
      return this.parity;
    }

    /**
     * Returns the current value of stopBits.
     * 
     * @return the stopBits
     */
    public StopBits getStopBits()
    {
      return this.stopBits;
    }

    /**
     * Returns the current value of inverted.
     * 
     * @return the inverted
     */
    public boolean isInverted()
    {
      return this.inverted;
    }

    /**
     * Returns whether the least-significant bit is sent first, or last.
     * 
     * @return <code>true</code> if the least-significant bit is sent first,
     *         <code>false</code> if it is sent last.
     */
    public boolean isLeastSignificantBitFirst()
    {
      return this.lsbFirst;
    }
  }

  /**
   * Denotes a callback that is used to report the decoding results and/or
   * errors back.
   */
  public static interface SerialDecoderCallback
  {
    // METHODS

    /**
     * Called when a frame error occurred, due to an invalid number of stop-bits
     * or an incorrect value for a single stop-bit.
     * 
     * @param aTime
     *          the time stamp on which the frame error occurred.
     */
    void reportFrameError( long aTime );

    /**
     * Called when a parity error occurred for a data symbol.
     * 
     * @param aTime
     *          the time stamp on which the parity error occurred.
     */
    void reportParityError( long aTime );

    /**
     * Called when a start error occurred, due to an invalid level for a
     * start-bit.
     * 
     * @param aTime
     *          the time stamp on which the start error occurred.
     */
    void reportStartError( long aTime );

    /**
     * Called when a single data symbol has been fully decoded.
     * 
     * @param aSymbol
     *          the actual data symbol, as integer value. For 8-bit symbols, it
     *          can be directly represented as ASCII.
     * @param aStartTime
     *          the starting time stamp on which the data symbol starts;
     * @param aEndTime
     *          the ending time stamp on which the data symbol ends;
     */
    void reportSymbol( int aSymbol, long aStartTime, long aEndTime );
  }

  /**
   * Denotes the number of stop bits, as used in serial protocols to indicate an
   * end-of-frame.
   */
  public static enum StopBits
  {
    // CONSTANTS

    /** One stop bit. */
    ONE,
    /** One and a half stop bit. */
    ONE_HALF,
    /** Two stop bits. */
    TWO;

    // METHODS

    /**
     * Returns the number of stop bits as numeric value.
     * 
     * @return a stop bit count, either 1, 2 or 1.5.
     */
    public double getValue()
    {
      if ( this == ONE_HALF )
      {
        return 1.5;
      }
      else if ( this == TWO )
      {
        return 2.0;
      }

      return 1.0;
    }
  }

  // VARIABLES

  private final SerialConfiguration configuration;
  private final ToolContext context;

  private ToolProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AsyncSerialDataDecoder} instance.
   * 
   * @param aConfiguration
   *          the configuration to use, cannot be <code>null</code>.
   */
  public AsyncSerialDataDecoder( final SerialConfiguration aConfiguration, final ToolContext aContext )
  {
    this.configuration = aConfiguration;
    this.context = aContext;
  }

  // METHODS

  /**
   * Decodes a serial data line.
   * 
   * @param aDataSet
   *          the acquisition data to decode as serial data, cannot be
   *          <code>null</code>;
   * @param aChannelIndex
   *          the channel index to decode, >= 0;
   * @return the number of decoded symbols, >= 0.
   */
  public int decodeDataLine( final AcquisitionResult aDataSet, final int aChannelIndex,
      final SerialDecoderCallback aCallback )
  {
    final int frameSize = this.configuration.getFrameSize( aDataSet.getSampleRate() );
    final int bitLength = this.configuration.getBitLength( aDataSet.getSampleRate() );
    final int bitCount = this.configuration.getDataBits();

    final StopBits stopBits = this.configuration.getStopBits();
    final Parity parity = this.configuration.getParity();

    final int bitCenter = bitLength / 2;
    final int mask = ( 1 << aChannelIndex );

    final long[] timestamps = aDataSet.getTimestamps();

    final long startOfDecode = timestamps[this.context.getStartSampleIndex()];
    final long endOfDecode = timestamps[this.context.getEndSampleIndex()];

    long time = Math.max( 0, startOfDecode );

    setProgress( 0 );

    int symbolCount = 0;
    while ( ( endOfDecode - time ) > frameSize )
    {
      /*
       * find first falling edge this is the start of the startbit. If the
       * signal is inverted, find the first rising edge.
       */
      time = findStartBit( time, endOfDecode, mask );
      if ( time < 0 )
      {
        // No more start bits; stop the decoding process...
        break;
      }

      // Sampling is done in the middle of each bit the start bit must be low.
      // If the signal is inverted, the startbit must be high.
      time += bitCenter;
      if ( !isSpace( time, mask ) )
      {
        // this is not a start bit !
        aCallback.reportStartError( time );
      }

      // Keep track of where the symbol originally started; note that we're
      // shifting time from the bit-*center* onwards, so we need to add only
      // half a bit to the time to get the starting timestamp of the symbol...
      final long startTime = time + bitCenter;
      // The end time of a symbol can be calculated easily from the start
      // timestamp...
      final long endTime = ( startTime + ( bitCount * bitLength ) ) - 1;

      int symbol = 0;
      for ( int bitIdx = 0; bitIdx < bitCount; bitIdx++ )
      {
        time += bitLength;
        if ( isMark( time, mask ) )
        {
          symbol |= ( 1 << bitIdx );
        }
      }

      // Post-process the data...
      symbol = correctSymbol( symbol, bitCount );

      // fully decoded a single symbol...
      aCallback.reportSymbol( symbol, startTime, endTime );
      symbolCount++;

      // Sample parity bit (if available/desired).
      if ( parity.isOdd() || parity.isEven() )
      {
        final int actualBitCount = Integer.bitCount( symbol );
        // determine which parity bit we should expect...
        final int expectedValue;
        if ( parity.isOdd() )
        {
          expectedValue = ( actualBitCount % 2 ) == 0 ? mask : 0;
        }
        else
        /* if ( parity.isEven() ) */
        {
          expectedValue = ( actualBitCount % 2 ) == 1 ? mask : 0;
        }

        time += bitLength; // = middle of the parity bit...
        if ( !isExpectedLevel( time, mask, expectedValue ) )
        {
          aCallback.reportParityError( time );
        }
      }

      // Sample stopbit(s)
      time += bitLength; // = middle of first stop bit...

      double stopBitCount = stopBits.getValue();
      while ( stopBitCount > 0.0 )
      {
        if ( !isMark( time, mask ) )
        {
          aCallback.reportFrameError( time );
        }
        stopBitCount -= ( ( stopBitCount > 1.0 ) ? 1.0 : stopBitCount );

        time += stopBitCount * bitLength;
      }

      setProgress( getPercentage( time, startOfDecode, endOfDecode ) );
    }

    setProgress( 100 );

    return symbolCount;
  }

  /**
   * Sets progressListener to the given value.
   * 
   * @param aProgressListener
   *          the progressListener to set.
   */
  public void setProgressListener( final ToolProgressListener aProgressListener )
  {
    this.progressListener = aProgressListener;
  }

  /**
   * Corrects the symbol conform the settings of this decoder, for example
   * whether or not the entire symbol is inverted, or which bit-order it should
   * have.
   * 
   * @param aSymbol
   *          the original symbol to correct;
   * @param aBitCount
   *          the number of bits in the symbol.
   * @return the corrected symbol, can be equal to the original one.
   */
  private int correctSymbol( int aSymbol, final int aBitCount )
  {
    // Issue #85: invert & reverse the bit order...
    if ( isInverted() )
    {
      aSymbol = ~aSymbol & getBitMask( aBitCount );
    }

    // If the least significant bit is first, we need to swap bit-order, as we
    // normally represent the bits with the most significant bit first...
    if ( this.configuration.isLeastSignificantBitFirst() )
    {
      aSymbol = reverseBits( aSymbol, aBitCount );
    }

    return aSymbol;
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
  private long findStartBit( final long aStartOfDecode, final long aEndOfDecode, final int aMask )
  {
    final Edge sampleEdge = isInverted() ? Edge.RISING : Edge.FALLING;

    long result = -1;

    int oldBitValue = getDataValue( aStartOfDecode, aMask );
    for ( long timeCursor = aStartOfDecode + 1; ( result < 0 ) && ( timeCursor < aEndOfDecode ); timeCursor++ )
    {
      final int bitValue = getDataValue( timeCursor, aMask );

      Edge edge = Edge.toEdge( oldBitValue, bitValue );
      if ( sampleEdge == edge )
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
  private int getDataValue( final long aTimeValue, final int aMask )
  {
    final AcquisitionResult data = this.context.getData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    int i;
    for ( i = 1; i < timestamps.length; i++ )
    {
      if ( aTimeValue < timestamps[i] )
      {
        break;
      }
    }

    int value = values[i - 1];
    if ( isInverted() )
    {
      return ~value & aMask;
    }

    return value & aMask;
  }

  /**
   * Returns whether the bit-value (denoted by the given mask) is the given
   * expected mask.
   * <p>
   * In case the signal is inverted, this method will inverse the check.
   * </p>
   * 
   * @param aTimestamp
   *          the timestamp to sample the data at;
   * @param aMask
   *          the bit-value mask;
   * @param aExpectedMask
   *          the expected bit-value mask.
   * @return <code>true</code> if the given bit-value meets the expected mask,
   *         <code>false</code> otherwise.
   * @see #isInverted()
   */
  private boolean isExpectedLevel( final long aTimestamp, final int aMask, final int aExpectedMask )
  {
    return getDataValue( aTimestamp, aMask ) == aExpectedMask;
  }

  /**
   * Returns whether the entire signal is inverted.
   * 
   * @return <code>true</code> if the signal is to be considered inverted,
   *         <code>false</code> otherwise.
   */
  private boolean isInverted()
  {
    return this.configuration.isInverted();
  }

  /**
   * Returns whether the (data-)value at the given timestamp is at a 'mark'
   * (active high, or -when inverted- active low).
   * 
   * @param aTimestamp
   *          the timestamp to sample the data at;
   * @param aMask
   *          the mask of the bit-value.
   * @return <code>true</code> if the bit-value is at the expected value,
   *         <code>false</code> otherwise.
   */
  private boolean isMark( final long aTimestamp, final int aMask )
  {
    return isExpectedLevel( aTimestamp, aMask, isInverted() ? 0x00 : aMask );
  }

  /**
   * Returns whether the (data-)value at the given timestamp is at a 'space'
   * (active low, or -when inverted- active high).
   * 
   * @param aTimestamp
   *          the timestamp to sample the data at;
   * @param aMask
   *          the mask of the bit-value.
   * @return <code>true</code> if the bit-value is at the expected value,
   *         <code>false</code> otherwise.
   */
  private boolean isSpace( final long aTimestamp, final int aMask )
  {
    return isExpectedLevel( aTimestamp, aMask, isInverted() ? aMask : 0x00 );
  }

  /**
   * @param aProgress
   */
  private void setProgress( final int aProgress )
  {
    if ( this.progressListener != null )
    {
      this.progressListener.setProgress( aProgress );
    }
  }

}
