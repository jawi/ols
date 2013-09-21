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

import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a generic decoder for asynchronous serial data, such as used for
 * UARTs, smartcards, LIN and other protocols.
 */
public class AsyncSerialDataDecoder
{
  // INNER TYPES

  /**
   * Denotes the type of error that can occur during decoding.
   */
  public static enum ErrorType
  {
    /** Denotes an unexpected value for the start bit. */
    START,
    /** Denotes the symbol has a parity error in it. */
    PARITY,
    /**
     * Denotes a frame error, due to an invalid number of stop-bits or an
     * incorrect value for a single stop-bit
     */
    FRAME;
  }

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
   * Denotes bit order used to transmit bits over a serial line.
   */
  public static enum BitOrder
  {
    LSB_FIRST, // This is the most common variant
    MSB_FIRST;
  }

  /**
   * Denotes bit order used to transmit bits over a serial line.
   */
  public static enum BitEncoding
  {
    HIGH_IS_MARK, // This is the most common variant, high = 1, low = 0
    HIGH_IS_SPACE; // high = 0, low = 1
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
    private final BitEncoding bitEncoding;
    private final BitOrder bitOrder;
    private final BitLevel idleLevel;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SerialConfiguration} instance, defaulting to 8N1,
     * 9600 baud, no inversion of the signal and least-significant bit first.
     */
    public SerialConfiguration()
    {
      this( 9600, 8, StopBits.ONE, Parity.NONE, BitEncoding.HIGH_IS_MARK, BitOrder.LSB_FIRST, BitLevel.HIGH );
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
     * @param aBitEncoding
     *          what bit encoding is used;
     * @param aBitOrder
     *          what bit order is used.
     */
    public SerialConfiguration( final int aBaudRate, final int aDataBits, final StopBits aStopBits,
        final Parity aParity, final BitEncoding aBitEncoding, final BitOrder aBitOrder, final BitLevel aIdleLevel )
    {
      this.baudRate = aBaudRate;
      this.dataBits = aDataBits;
      this.stopBits = aStopBits;
      this.parity = aParity;
      this.bitEncoding = aBitEncoding;
      this.bitOrder = aBitOrder;
      this.idleLevel = aIdleLevel;
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
     * Calculates the length for a single bit.
     * 
     * @param aSampleRate
     *          the sample rate to use, in Hertz, > 0.
     * @return bit length, in number of samples.
     */
    public double getBitLength( final int aSampleRate )
    {
      return ( ( ( double )aSampleRate ) / this.baudRate );
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
     * @return a frame size, in number of samples
     */
    public int getFrameSize( final int aSampleRate )
    {
      final double bitLength = getBitLength( aSampleRate );
      final int stopCount = ( int )Math.ceil( this.stopBits.getValue() );
      final int parityCount = this.parity.isNone() ? 0 : 1;
      return ( int )( ( this.dataBits + stopCount + parityCount ) * bitLength );
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
     * Returns the current value of bitEncoding.
     */
    public BitEncoding getBitEncoding()
    {
      return this.bitEncoding;
    }

    /**
     * Returns the current value of bitOrder.
     */
    public BitOrder getBitOrder()
    {
      return this.bitOrder;
    }

    /**
     * Returns the current value of idleLevel.
     */
    public BitLevel getIdleLevel()
    {
      return this.idleLevel;
    }
  }

  /**
   * Provides callbacks during the decoding of results, for example, to report
   * data and/or errors back.
   */
  public static interface SerialDecoderCallback
  {
    // METHODS

    /**
     * Called when a decoding error occurred.
     * 
     * @param aChannelIdx
     *          the channel index of on which the error occurred, >= 0;
     * @param aType
     *          the type of error that occurred, cannot be <code>null</code>;
     * @param aTime
     *          the time stamp on which the error occurred, >= 0.
     */
    void onError( int aChannelIdx, ErrorType aType, long aTime );

    /**
     * Called when an event occurs that is not related to a symbol or an error.
     * 
     * @param aChannelIdx
     *          the index of the channel on which the event occurs, >= 0;
     * @param aEvent
     *          the name of the event, cannot be <code>null</code>;
     * @param aStartTime
     *          the starting time stamp on which the event starts, >= 0;
     * @param aEndTime
     *          the ending time stamp on which the event ends, >= 0.
     */
    void onEvent( int aChannelIdx, String aEvent, long aStartTime, long aEndTime );

    /**
     * Called when a single data symbol has been fully decoded.
     * 
     * @param aChannelIdx
     *          the channel index of on which the symbol was decoded, >= 0;
     * @param aSymbol
     *          the actual data symbol, as integer value;
     * @param aStartTime
     *          the starting time stamp on which the data symbol starts, >= 0;
     * @param aEndTime
     *          the ending time stamp on which the data symbol ends, >= 0.
     */
    void onSymbol( int aChannelIdx, int aSymbol, long aStartTime, long aEndTime );
  }

  /**
   * High voltage vs low voltage.
   */
  public static enum BitLevel
  {
    HIGH, LOW;

    public BitLevel invert()
    {
      return ( this == HIGH ? LOW : HIGH );
    }

    public Edge nextEdge()
    {
      return ( this == HIGH ? Edge.FALLING : Edge.RISING );
    }
  }

  /**
   * Mark means a "1", space means a "0" (regardless of which is high and which
   * is low).
   */
  public static enum BitValue
  {
    SPACE, MARK;
  }

  /**
   * Helper class that can chop up a datastream into bits.
   */
  private class DataBitExtractor
  {
    // VARIABLES

    private double time;
    private int mask;
    private int channelIndex;
    private double bitLength;
    /** The number of samples that we've seen between two confirmed edges */
    private double confirmedSamples;
    /** The number of bits that we've seen between two confirmed edges */
    private long confirmedBits;
    /** The timestamp of the last edge we've seen */
    private double lastEdge;
    /** The number of bits we've processed since the last edge */
    private int bitsSinceEdge;

    // CONSTRUCTORS

    public DataBitExtractor( final int aChannelIndex )
    {
      this.time = 0;
      this.channelIndex = aChannelIndex;
      this.mask = ( 1 << aChannelIndex );
      this.bitLength = AsyncSerialDataDecoder.this.configuration.getBitLength( AsyncSerialDataDecoder.this.dataSet
          .getSampleRate() );
    }

    // METHODS

    /**
     * The level of the current bit (always the raw level, regardless of bit
     * encoding settings).
     */
    public BitLevel level()
    {
      final long halfTime = ( long )( this.time + ( this.bitLength / 2 ) );
      final int level = AsyncSerialDataDecoder.this.getDataValue( halfTime, this.mask );
      return ( level == 0 ? BitLevel.LOW : BitLevel.HIGH );
    }

    /**
     * The value of the current bit (this is its meaning depending on the bit
     * encoding, regardless of voltage levels).
     */
    public BitValue value()
    {
      if ( AsyncSerialDataDecoder.this.configuration.getBitEncoding() == BitEncoding.HIGH_IS_SPACE )
      {
        return ( level() == BitLevel.HIGH ? BitValue.SPACE : BitValue.MARK );
      }
      else
      {
        return ( level() == BitLevel.HIGH ? BitValue.MARK : BitValue.SPACE );
      }
    }

    /**
     * Skip over the current bit to the next one.
     */
    public void next()
    {
      this.time += this.bitLength;
      this.bitsSinceEdge++;
      final long start = ( long )( this.time - this.bitLength * 0.25 - 1 );
      final long end = ( long )( this.time + this.bitLength * 0.25 + 1 );

      // Find an edge in the area where we would expect one
      final long edge = AsyncSerialDataDecoder.this.findEdge( this.channelIndex, Edge.NONE, start, end );
      if ( edge >= 0 )
      {
        // Found an edge, skip to that timestamp instead.
        this.time = edge;

        // Add the bits since the last edge to the average
        this.confirmedSamples += ( this.time - this.lastEdge );
        this.confirmedBits += this.bitsSinceEdge;
        // And reset the last edge
        this.lastEdge = this.time;
        this.bitsSinceEdge = 0;
      }
    }

    /**
     * Jump to the bit starting at the given time (sample number).
     */
    public void jumpTo( long time )
    {
      this.time = time;
      /**
       * Assume we're jumping here because our caller found an edge.
       */
      this.lastEdge = time;
      this.bitsSinceEdge = 0;
    }

    /**
     * The sample time of the start of the current bit.
     */
    public long time()
    {
      return ( long )( this.time );
    }

    /**
     * The average bit length for sequences of bits found between two edges
     * (e.g., the ones of which we can be fairly certain of their length, unlike
     * for example trailing MARK bits, which blend into the stop bits and any
     * idle time after the byte).
     */
    public double averageBitLength()
    {
      return ( this.confirmedSamples / this.confirmedBits );
    }
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

  // CONSTANTS

  public static final int[] COMMON_BAUDRATES = { 150, 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 28800, 38400,
      57600, 76800, 115200, 230400, 460800, 921600 };

  // VARIABLES

  protected final SerialConfiguration configuration;
  protected final AcquisitionResult dataSet;
  protected final ToolContext context;

  private SerialDecoderCallback callback;
  private ToolProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AsyncSerialDataDecoder} instance.
   * 
   * @param aConfiguration
   *          the configuration to use, cannot be <code>null</code>;
   * @param aContext
   *          the tool context to use, cannot be <code>null</code>.
   */
  public AsyncSerialDataDecoder( final SerialConfiguration aConfiguration, final ToolContext aContext )
  {
    this.configuration = aConfiguration;
    this.context = aContext;
    this.dataSet = aContext.getData();
  }

  // METHODS

  /**
   * Finds the sample index of the given timestamp value.
   * <p>
   * Note the sample index returned is <em>not per se</em> equal to
   * <code>timestamps[result]</code>!
   * </p>
   * 
   * @param aTimeValue
   *          the time value to search the corresponding index for, >= 0.
   * @return a sample index, >= 0.
   */
  protected static final int findSampleIndex( final long[] aTimestamps, final long aTimeValue )
  {
    int k = Arrays.binarySearch( aTimestamps, aTimeValue );
    if ( k < 0 )
    {
      k = -( k + 1 );
    }
    return k;
  }

  /**
   * Decodes a serial data line.
   * 
   * @param aChannelIndex
   *          the channel index to decode, >= 0;
   * @return the bit length used in decoding, in number of samples, >= 0;
   */
  public double decodeDataLine( final int aChannelIndex )
  {
    final int frameSize = this.configuration.getFrameSize( this.dataSet.getSampleRate() );
    final double bitLength = this.configuration.getBitLength( this.dataSet.getSampleRate() );
    final int bitCount = this.configuration.getDataBits();

    final StopBits stopBits = this.configuration.getStopBits();
    final Parity parity = this.configuration.getParity();

    final long[] timestamps = this.dataSet.getTimestamps();

    final long startOfDecode = timestamps[this.context.getStartSampleIndex()];
    final long endOfDecode = timestamps[this.context.getEndSampleIndex()];
    final BitLevel idleLevel = this.configuration.getIdleLevel();

    DataBitExtractor extractor = new DataBitExtractor( aChannelIndex );
    setProgress( 0 );

    long start = findStartBit( aChannelIndex, idleLevel.nextEdge(), startOfDecode, endOfDecode );
    while ( ( start >= 0 ) && ( ( endOfDecode - start ) > frameSize ) )
    {
      extractor.jumpTo( start );

      if ( ( extractor.level() != idleLevel.invert() ) && ( this.callback != null ) )
      {
        // this is not a start bit !
        this.callback.onError( aChannelIndex, ErrorType.START, extractor.time() );
      }
      extractor.next();

      // Keep track of where the symbol originally started;
      final long startTime = extractor.time();

      int symbol = 0;
      int marks = 0;
      for ( int bitIdx = 0; bitIdx < bitCount; bitIdx++ )
      {
        if ( extractor.value() == BitValue.MARK )
        {
          symbol |= ( 1 << bitIdx );
          marks++;
        }
        extractor.next();
      }
      final long endTime = extractor.time() - 1;

      // If the most significant bit is first, we need to swap bit-order, as we
      // normally represent the bits with the least significant bit first...
      if ( this.configuration.getBitOrder() == BitOrder.MSB_FIRST )
      {
        symbol = reverseBits( symbol, bitCount );
      }

      // fully decoded a single symbol...
      if ( this.callback != null )
      {
        this.callback.onSymbol( aChannelIndex, symbol, startTime, endTime );
      }

      // Sample parity bit (if available/desired).
      if ( parity.isOdd() || parity.isEven() )
      {
        if ( extractor.value() == BitValue.MARK )
        {
          marks++;
        }

        // Even parity means total number of marks (including the parity
        // bit) should be even, odd means they should be odd.
        if ( ( parity.isOdd() && ( marks % 2 == 0 ) ) || ( parity.isEven() && ( marks % 2 == 1 ) ) )
        {
          this.callback.onError( aChannelIndex, ErrorType.PARITY, extractor.time() );
        }

        extractor.next();
      }

      // Check value of stopbit
      if ( ( extractor.level() != idleLevel ) && ( this.callback != null ) )
      {
        this.callback.onError( aChannelIndex, ErrorType.FRAME, extractor.time() );
      }

      // Find start bit after the stop bit
      start = findStartBit( aChannelIndex, idleLevel.nextEdge(), ( long )( extractor.time() + ( bitLength / 2 ) ),
          endOfDecode );

      // Check length of stopbit
      final long endOfStopbit = extractor.time() + ( long )( stopBits.getValue() * bitLength );
      if ( start >= 0 && ( endOfStopbit > start ) && ( this.callback != null ) )
      {
        this.callback.onError( aChannelIndex, ErrorType.FRAME, extractor.time() );
      }

      if ( start >= 0 )
      {
        setProgress( getPercentage( start, startOfDecode, endOfDecode ) );
      }
    }

    setProgress( 100 );

    return extractor.averageBitLength();
  }

  /**
   * Sets the decoder callback.
   * 
   * @param aCallback
   *          the callback to set, can be <code>null</code> in case no callbacks
   *          are needed.
   */
  public void setCallback( final SerialDecoderCallback aCallback )
  {
    this.callback = aCallback;
  }

  /**
   * Sets the progress listener.
   * 
   * @param aProgressListener
   *          the progress listener to set, can be <code>null</code> in case no
   *          progress reporting is wanted.
   */
  public final void setProgressListener( final ToolProgressListener aProgressListener )
  {
    this.progressListener = aProgressListener;
  }

  /**
   * Finds a certain type of edge on a channel between the two given timestamps.
   * 
   * @param aChannelIndex
   *          the index of the channel to find the start bit on;
   * @param aSampleEdge
   *          the edge to find, Edge.NONE for any edge;
   * @param aStartOfDecode
   *          the timestamp to start searching;
   * @param aEndOfDecode
   *          the timestamp to end the search;
   * @return the time at which the start bit was found, -1 if it is not found.
   */
  protected final long findEdge( final int aChannelIndex, final Edge aSampleEdge, final long aStartOfDecode,
      final long aEndOfDecode )
  {
    final int mask = ( 1 << aChannelIndex );
    long result = -1;

    int oldBitValue = getDataValue( aStartOfDecode, mask );
    for ( long timeCursor = aStartOfDecode + 1; ( result < 0 ) && ( timeCursor < aEndOfDecode ); timeCursor++ )
    {
      final int bitValue = getDataValue( timeCursor, mask );

      Edge edge = Edge.toEdge( oldBitValue, bitValue );
      if ( aSampleEdge.isNone() && !edge.isNone() )
      {
        /* No edge given, so any edge will do */
        result = timeCursor;
      }
      else if ( !aSampleEdge.isNone() && ( aSampleEdge == edge ) )
      {
        result = timeCursor;
      }

      oldBitValue = bitValue;
    }

    return result;
  }

  /**
   * Find first edge of the given type.
   * 
   * @param aChannelIndex
   *          the index of the channel to find the start bit on;
   * @param aEdge
   *          the type of edge to look for;
   * @param aStartOfDecode
   *          the timestamp to start searching;
   * @param aEndOfDecode
   *          the timestamp to end the search;
   * @return the time at which the start bit was found, -1 if it is not found.
   */
  protected long findStartBit( final int aChannelIndex, final Edge aEdge, final long aStartOfDecode,
      final long aEndOfDecode )
  {
    return findEdge( aChannelIndex, aEdge, aStartOfDecode, aEndOfDecode );
  }

  /**
   * Sets the decoder helper.
   * 
   * @return a decoder helper, can be <code>null</code> if no such helper is
   *         set.
   */
  protected final SerialDecoderCallback getCallback()
  {
    return this.callback;
  }

  /**
   * Returns the data value for the given time stamp.
   * 
   * @param aTimeValue
   *          the time stamp to return the data value for.
   * @return the data value of the sample index right before the given time
   *         value.
   */
  protected final int getDataValue( final long aTimeValue, final int aMask )
  {
    final int[] values = this.dataSet.getValues();
    final long[] timestamps = this.dataSet.getTimestamps();
    int k = findSampleIndex( timestamps, aTimeValue );

    int value = ( ( k == 0 ) ? values[0] : values[k - 1] );

    return value & aMask;
  }

  /**
   * @param aProgress
   */
  protected final void setProgress( final int aProgress )
  {
    if ( this.progressListener != null )
    {
      this.progressListener.setProgress( aProgress );
    }
  }
}
