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
package nl.lxtreme.ols.tool.uart;


import static nl.lxtreme.ols.util.NumberUtils.*;

import java.util.logging.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.annotation.*;
import nl.lxtreme.ols.util.*;


/**
 * @author jajans
 */
public class UARTAnalyserTask implements ToolTask<UARTDataSet>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( UARTAnalyserTask.class.getName() );

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final AnnotationListener annotationListener;

  private int rxdIndex;
  private int txdIndex;
  private int ctsIndex;
  private int rtsIndex;
  private int dcdIndex;
  private int riIndex;
  private int dsrIndex;
  private int dtrIndex;
  private boolean inverted;
  private boolean inversed;
  private UARTStopBits stopBits;
  private UARTParity parity;
  private int bitCount;
  private int baudRate;

  // CONSTRUCTORS

  /**
   * @param aContext
   * @param aAnnotationListener
   */
  public UARTAnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annotationListener = aAnnotationListener;

    this.rxdIndex = -1;
    this.txdIndex = -1;
    this.ctsIndex = -1;
    this.rtsIndex = -1;
    this.dcdIndex = -1;
    this.riIndex = -1;
    this.dsrIndex = -1;
    this.dtrIndex = -1;
    this.baudRate = -1;
  }

  // METHODS

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public UARTDataSet call() throws Exception
  {
    final AcquisitionResult data = this.context.getData();

    /*
     * Start decode from trigger or if no trigger is available from the first
     * falling edge. The decoder works with two independant decoder runs. First
     * for RxD and then for TxD, after this CTS, RTS, etc. is detected if
     * enabled. After decoding all the decoded data are unsortet before the data
     * is displayed it must be sortet by time.
     */

    final int[] values = data.getValues();

    int startOfDecode = this.context.getStartSampleIndex();
    final int endOfDecode = this.context.getEndSampleIndex();

    // find first state change on the selected lines
    final int mask = getBitMask();

    final int value = values[startOfDecode] & mask;
    for ( int i = startOfDecode + 1; i < endOfDecode; i++ )
    {
      if ( value != ( values[i] & mask ) )
      {
        startOfDecode = i;
        break;
      }
    }

    startOfDecode = Math.max( 0, startOfDecode - 10 );

    // Make sure we've got a valid range to decode..
    if ( startOfDecode >= endOfDecode )
    {
      LOG.log( Level.WARNING, "No valid data range found for UART analysis! Analysis aborted..." );
      throw new IllegalStateException( "No valid data range found for UART analysis!" );
    }

    final UARTDataSet decodedData = new UARTDataSet( startOfDecode, endOfDecode, data );

    // decode RxD/TxD data lines...
    if ( this.rxdIndex >= 0 )
    {
      prepareAndDecodeData( decodedData, this.rxdIndex, UARTData.UART_TYPE_RXDATA, UARTDataSet.UART_RXD );
    }
    if ( this.txdIndex >= 0 )
    {
      prepareAndDecodeData( decodedData, this.txdIndex, UARTData.UART_TYPE_TXDATA, UARTDataSet.UART_TXD );
    }

    // decode control lines...
    if ( this.ctsIndex >= 0 )
    {
      prepareAndDecodeControl( decodedData, this.ctsIndex, UARTDataSet.UART_CTS );
    }
    if ( this.rtsIndex >= 0 )
    {
      prepareAndDecodeControl( decodedData, this.rtsIndex, UARTDataSet.UART_RTS );
    }
    if ( this.dcdIndex >= 0 )
    {
      prepareAndDecodeControl( decodedData, this.dcdIndex, UARTDataSet.UART_DCD );
    }
    if ( this.riIndex >= 0 )
    {
      prepareAndDecodeControl( decodedData, this.riIndex, UARTDataSet.UART_RI );
    }
    if ( this.dsrIndex >= 0 )
    {
      prepareAndDecodeControl( decodedData, this.dsrIndex, UARTDataSet.UART_DSR );
    }
    if ( this.dtrIndex >= 0 )
    {
      prepareAndDecodeControl( decodedData, this.dtrIndex, UARTDataSet.UART_DTR );
    }

    // sort the results by time
    decodedData.sort();

    return decodedData;
  }

  /**
   * Returns whether the entire signal is inversed.
   * 
   * @return <code>true</code> if the signal is to be considered inversed,
   *         <code>false</code> otherwise.
   */
  public boolean isInversed()
  {
    return this.inversed;
  }

  /**
   * Returns whether the entire signal is inverted.
   * 
   * @return <code>true</code> if the signal is to be considered inverted,
   *         <code>false</code> otherwise.
   */
  public boolean isInverted()
  {
    return this.inverted;
  }

  /**
   * Sets baudRate to the given value.
   * 
   * @param aBaudRate
   *          the baudRate to set.
   */
  public void setBaudRate( final int aBaudRate )
  {
    this.baudRate = aBaudRate;
  }

  /**
   * @param aBitCount
   */
  public void setBitCount( final int aBitCount )
  {
    this.bitCount = aBitCount;
  }

  /**
   * @param aCtsIndex
   *          the ctsMask to set
   */
  public void setCtsIndex( final int aCtsIndex )
  {
    this.ctsIndex = aCtsIndex;
  }

  /**
   * @param aDcdMask
   *          the dcdMask to set
   */
  public void setDcdIndex( final int aDcdIndex )
  {
    this.dcdIndex = aDcdIndex;
  }

  /**
   * @param aDsrMask
   *          the dsrMask to set
   */
  public void setDsrIndex( final int aDsrIndex )
  {
    this.dsrIndex = aDsrIndex;
  }

  /**
   * @param aDtrMask
   *          the dtrMask to set
   */
  public void setDtrIndex( final int aDtrIndex )
  {
    this.dtrIndex = aDtrIndex;
  }

  /**
   * @param aInversed
   */
  public void setInversed( final boolean aInversed )
  {
    this.inversed = aInversed;
  }

  /**
   * @param aInverted
   */
  public void setInverted( final boolean aInverted )
  {
    this.inverted = aInverted;
  }

  /**
   * @param aParity
   */
  public void setParity( final UARTParity aParity )
  {
    this.parity = aParity;
  }

  /**
   * @param aRiMask
   *          the riMask to set
   */
  public void setRiIndex( final int aRiIndex )
  {
    this.riIndex = aRiIndex;
  }

  /**
   * @param aRtsMask
   *          the rtsMask to set
   */
  public void setRtsIndex( final int aRtsIndex )
  {
    this.rtsIndex = aRtsIndex;
  }

  /**
   * @param aRxdMask
   *          the rxdMask to set
   */
  public void setRxdIndex( final int aRxdIndex )
  {
    this.rxdIndex = aRxdIndex;
  }

  /**
   * @param aStopBits
   */
  public void setStopBits( final UARTStopBits aStopBits )
  {
    this.stopBits = aStopBits;
  }

  /**
   * @param aTxdMask
   *          the txdMask to set
   */
  public void setTxdIndex( final int aTxdIndex )
  {
    this.txdIndex = aTxdIndex;
  }

  /**
   * Factory method for creating the baud rate analyzer for the given
   * acquisition results & bit mask.
   * 
   * @param aData
   *          the acquisition results to use;
   * @param aMask
   *          the bit mask of the data to use.
   * @return a {@link BaudRateAnalyzer} instance, never <code>null</code>.
   */
  private BaudRateAnalyzer createBaudRateAnalyzer( final AcquisitionResult aData, final int aMask )
  {
    if ( this.baudRate <= 0 )
    {
      // Auto detect the baud rate...
      return new BaudRateAnalyzer( aData.getSampleRate(), aData.getValues(), aData.getTimestamps(), aMask );
    }
    // Use a fixed baud rate...
    return new BaudRateAnalyzer( aData.getSampleRate(), this.baudRate );
  }

  /**
   * Decodes a control line.
   * 
   * @param aDataSet
   *          the data set to add the decoded data to;
   * @param aChannelIndex
   *          the channel index of the control-line to decode;
   * @param aName
   *          the name of the control line to decode.
   */
  private void decodeControl( final UARTDataSet aDataSet, final int aChannelIndex, final String aName )
  {
    final AcquisitionResult data = this.context.getData();

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, "Decoding control: {0} ...", aName );
    }

    final int mask = ( 1 << aChannelIndex );

    final int startSampleIdx = aDataSet.getStartOfDecode();
    final int endSampleIdx = aDataSet.getEndOfDecode();

    final int[] values = data.getValues();
    this.progressListener.setProgress( 0 );

    int oldValue = values[startSampleIdx] & mask;
    for ( int i = startSampleIdx + 1; i < endSampleIdx; i++ )
    {
      final int value = values[i] & mask;

      final Edge edge = Edge.toEdge( oldValue, value );
      if ( edge.isRising() )
      {
        aDataSet.reportControlHigh( aChannelIndex, i, aName );
      }
      if ( edge.isFalling() )
      {
        aDataSet.reportControlLow( aChannelIndex, i, aName );
      }
      oldValue = value;

      // update progress
      this.progressListener.setProgress( getPercentage( i, startSampleIdx, endSampleIdx ) );
    }
  }

  /**
   * @param aDataSet
   *          the data set to add the decoded data to;
   * @param aChannelIndex
   *          the channel index to decode;
   * @param aType
   *          type of the data (rx or tx)
   */
  private void decodeData( final UARTDataSet aDataSet, final int aChannelIndex, final int aEventType )
  {
    final AcquisitionResult data = this.context.getData();

    final int mask = ( 1 << aChannelIndex );
    final BaudRateAnalyzer baudrateAnalyzer = createBaudRateAnalyzer( data, mask );

    final int bitLength = baudrateAnalyzer.getBestBitLength();

    LOG.log( Level.FINE, "Baudrate = {0}bps", Integer.valueOf( bitLength ) );

    if ( bitLength <= 0 )
    {
      LOG.log( Level.INFO, "No (usable) {0}-data found for determining bitlength/baudrate ...",
          aChannelIndex == this.rxdIndex ? UARTDataSet.UART_RXD : UARTDataSet.UART_TXD );
    }
    else
    {
      // We know the avg. bitlength, so we can use it for calculating the
      // baudrate...
      aDataSet.setSampledBitLength( bitLength );

      aDataSet.setBaudRateExact( baudrateAnalyzer.getBaudRateExact() );
      aDataSet.setBaudRate( baudrateAnalyzer.getBaudRate() );

      if ( LOG.isLoggable( Level.FINE ) )
      {
        LOG.fine( "Samplerate: " + data.getSampleRate() + ", bitlength: " + bitLength + ", baudrate = "
            + aDataSet.getBaudRate() );
      }

      decodeDataLine( aDataSet, aChannelIndex, bitLength, aEventType );
    }
  }

  /**
   * decode a UART data line
   * 
   * @param aDataSet
   *          the data set to add the decoded data to;
   * @param aChannelIndex
   *          the channel index to decode;
   * @param aBitLength
   *          the length of a single bit (counted samples per bit)
   * @param aType
   *          type of the data (rx or tx)
   * @return the number of decoded symbols, >= 0.
   */
  private int decodeDataLine( final UARTDataSet aDataSet, final int aChannelIndex, final int aBitLength, final int aType )
  {
    final AcquisitionResult data = this.context.getData();

    final int mask = ( 1 << aChannelIndex );
    final int stopCount = ( int )Math.ceil( this.stopBits.getValue() );
    final int parityCount = this.parity.isNone() ? 0 : 1;
    final int frameSize = ( this.bitCount + stopCount + parityCount ) * aBitLength;

    final int bitCenter = aBitLength / 2;

    final long[] timestamps = data.getTimestamps();

    final long startOfDecode = timestamps[aDataSet.getStartOfDecode()];
    final long endOfDecode = timestamps[aDataSet.getEndOfDecode()];

    long time = Math.max( 0, startOfDecode );
    this.progressListener.setProgress( 0 );

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
        LOG.log( Level.INFO, "Decoding ended for {0}; no start bit found...",
            ( aChannelIndex == this.rxdIndex ) ? UARTDataSet.UART_RXD : UARTDataSet.UART_TXD );
        break;
      }

      /*
       * Sampling is done in the middle of each bit the start bit must be low.
       * If the signal is inverted, the startbit must be high.
       */
      time += bitCenter;
      if ( !isSpace( time, mask ) )
      {
        // this is not a start bit !
        reportStartError( data, aDataSet, time, aType );
      }

      /*
       * sample the databits in the middle of the bit position
       */
      int value = 0;

      final long startTime = time + aBitLength;
      for ( int bitIdx = 0; bitIdx < this.bitCount; bitIdx++ )
      {
        time += aBitLength;
        if ( isMark( time, mask ) )
        {
          value |= ( 1 << bitIdx );
        }
      }

      // inverse value; actually meaning invert + MSB first, iso LSB first...
      if ( isInversed() )
      {
        // Issue #85: reverse the value and swap the bit order...
        // TODO we should coerse the #isInversed() & #isInverted(), and
        // introduce a new property #getBitOrder()...
        value = NumberUtils.reverseBits( value ^ 0xFF, this.bitCount );
      }

      // fully decoded a single symbol...
      reportData( data, aDataSet, aChannelIndex, startTime, time, value, aType );
      symbolCount++;

      /*
       * Sample parity bit (if available/desired).
       */
      if ( isOddParity() || isEvenParity() )
      {
        time += aBitLength;

        final boolean evenBitCount = ( Integer.bitCount( value ) & 1 ) == 0;
        // determine which parity bit we should expect...
        final int expectedValue;
        if ( isOddParity() )
        {
          expectedValue = evenBitCount ? mask : 0;
        }
        else
        {
          expectedValue = evenBitCount ? 0 : mask;
        }

        if ( !isExpectedLevel( time, mask, expectedValue ) )
        {
          reportParityError( data, aDataSet, time, aType );
        }
      }

      /*
       * sample stopbit(s)
       */
      time += aBitLength;

      double stopBitCount = this.stopBits.getValue();
      while ( stopBitCount > 0.0 )
      {
        if ( !isMark( time, mask ) )
        {
          reportFrameError( data, aDataSet, time, aType );
        }
        stopBitCount -= 1.0;

        time += stopBitCount * aBitLength;
      }

      this.progressListener.setProgress( getPercentage( time, startOfDecode, endOfDecode ) );
    }

    this.progressListener.setProgress( 100 );

    return symbolCount;
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

    int oldBitValue = getDataValue( aStartOfDecode ) & aMask;
    for ( long timeCursor = aStartOfDecode + 1; ( result < 0 ) && ( timeCursor < aEndOfDecode ); timeCursor++ )
    {
      final int bitValue = getDataValue( timeCursor ) & aMask;

      final Edge edge = Edge.toEdge( oldBitValue, bitValue );
      if ( sampleEdge == edge )
      {
        result = timeCursor;
      }

      oldBitValue = bitValue;
    }

    return result;
  }

  /**
   * Builds a bit mask that can be applied to the data to filter out only the
   * interesting channels.
   * 
   * @return a bit mask, >= 0.
   */
  private int getBitMask()
  {
    int result = 0x00;
    if ( this.rxdIndex >= 0 )
    {
      final int mask = ( 1 << this.rxdIndex );
      LOG.log( Level.FINE, "RxD mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.txdIndex >= 0 )
    {
      final int mask = ( 1 << this.txdIndex );
      LOG.log( Level.FINE, "TxD mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.ctsIndex >= 0 )
    {
      final int mask = ( 1 << this.ctsIndex );
      LOG.log( Level.FINE, "CTS mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.rtsIndex >= 0 )
    {
      final int mask = ( 1 << this.rtsIndex );
      LOG.log( Level.FINE, "RTS mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.dcdIndex >= 0 )
    {
      final int mask = ( 1 << this.dcdIndex );
      LOG.log( Level.FINE, "DCD mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.riIndex >= 0 )
    {
      final int mask = ( 1 << this.riIndex );
      LOG.log( Level.FINE, "RI mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.dsrIndex >= 0 )
    {
      final int mask = ( 1 << this.dsrIndex );
      LOG.log( Level.FINE, "DSR mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
    }
    if ( this.dtrIndex >= 0 )
    {
      final int mask = ( 1 << this.dtrIndex );
      LOG.log( Level.FINE, "DTR mask = 0x{0}", Integer.toHexString( mask ) );
      result |= mask;
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
    return values[i - 1];
  }

  /**
   * Returns whether an EVEN parity is chosen.
   * 
   * @return <code>true</code> if an even parity is chosen, <code>false</code>
   *         otherwise.
   */
  private boolean isEvenParity()
  {
    return this.parity == UARTParity.EVEN;
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
    final int value = getDataValue( aTimestamp ) & aMask;
    if ( isInverted() )
    {
      return value != aExpectedMask;
    }
    return value == aExpectedMask;
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
    return isExpectedLevel( aTimestamp, aMask, aMask );
  }

  /**
   * Returns whether an ODD parity is chosen.
   * 
   * @return <code>true</code> if an odd parity is chosen, <code>false</code>
   *         otherwise.
   */
  private boolean isOddParity()
  {
    return this.parity == UARTParity.ODD;
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
    return isExpectedLevel( aTimestamp, aMask, 0x00 );
  }

  /**
   * Prepares and decoded the control line indicated by the given channel index.
   * 
   * @param aDataSet
   *          the dataset to add the decoding results to;
   * @param aChannelIndex
   *          the channel index of the channel to decode;
   * @param aDefaultLabel
   *          the default label to use for the decoded channel.
   */
  private void prepareAndDecodeControl( final UARTDataSet aDataSet, final int aChannelIndex, final String aDefaultLabel )
  {
    prepareResult( aChannelIndex, aDefaultLabel );
    decodeControl( aDataSet, aChannelIndex, aDefaultLabel );
  }

  /**
   * Prepares and decoded the data line indicated by the given channel index.
   * 
   * @param aDataSet
   *          the dataset to add the decoding results to;
   * @param aChannelIndex
   *          the channel index of the channel to decode;
   * @param aEventType
   *          the event type to use for the decoded data;
   * @param aDefaultLabel
   *          the default label to use for the decoded channel.
   */
  private void prepareAndDecodeData( final UARTDataSet aDataSet, final int aChannelIndex, final int aEventType,
      final String aDefaultLabel )
  {
    prepareResult( aChannelIndex, aDefaultLabel );
    decodeData( aDataSet, aChannelIndex, aEventType );
  }

  /**
   * Determines the resulting channel label and clears any existing annotations.
   * 
   * @param aChannelIndex
   *          the channel index of the channel to prepare;
   * @param aLabel
   *          the default label to use for the channel (in case none is set).
   */
  private void prepareResult( final int aChannelIndex, final String aLabel )
  {
    this.annotationListener.clearAnnotations( aChannelIndex );
    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( aChannelIndex, aLabel ) );
  }

  /**
   * @param aDataSet
   * @param aChannelIndex
   * @param aByteValue
   * @param aType
   * @param aTimestamp
   */
  private void reportData( final AcquisitionResult aData, final UARTDataSet aDataSet, final int aChannelIndex,
      final long aStartTimestamp, final long aEndTimestamp, final int aByteValue, final int aType )
  {
    final int startSampleIdx = Math.max( aData.getSampleIndex( aStartTimestamp ), 0 );
    final int endSampleIdx = Math.min( aData.getSampleIndex( aEndTimestamp ) + 1, aData.getTimestamps().length - 1 );

    aDataSet.reportData( aChannelIndex, startSampleIdx, endSampleIdx, aByteValue, aType );

    this.annotationListener.onAnnotation( new SampleDataAnnotation( aChannelIndex, startSampleIdx, endSampleIdx, String
        .format( "0x%1$X (%1$c)", Integer.valueOf( aByteValue ) ) ) );
  }

  /**
   * @param aDataSet
   * @param aTimestamp
   * @param aType
   */
  private void reportFrameError( final AcquisitionResult aData, final UARTDataSet aDataSet, final long aTimestamp,
      final int aType )
  {
    if ( aType == UARTData.UART_TYPE_RXDATA )
    {
      aDataSet.reportFrameError( this.rxdIndex, aData.getSampleIndex( aTimestamp ), UARTData.UART_TYPE_RXEVENT );
    }
    else
    {
      aDataSet.reportFrameError( this.txdIndex, aData.getSampleIndex( aTimestamp ), UARTData.UART_TYPE_TXEVENT );
    }
  }

  /**
   * @param aDataSet
   * @param aTimestamp
   * @param aType
   */
  private void reportParityError( final AcquisitionResult aData, final UARTDataSet aDataSet, final long aTimestamp,
      final int aType )
  {
    if ( aType == UARTData.UART_TYPE_RXDATA )
    {
      aDataSet.reportParityError( this.rxdIndex, aData.getSampleIndex( aTimestamp ), UARTData.UART_TYPE_RXEVENT );
    }
    else
    {
      aDataSet.reportParityError( this.txdIndex, aData.getSampleIndex( aTimestamp ), UARTData.UART_TYPE_TXEVENT );
    }
  }

  /**
   * @param aDataSet
   * @param aTimestamp
   * @param aType
   */
  private void reportStartError( final AcquisitionResult aData, final UARTDataSet aDataSet, final long aTimestamp,
      final int aType )
  {
    if ( aType == UARTData.UART_TYPE_RXDATA )
    {
      aDataSet.reportStartError( this.rxdIndex, aData.getSampleIndex( aTimestamp ), UARTData.UART_TYPE_RXEVENT );
    }
    else
    {
      aDataSet.reportStartError( this.txdIndex, aData.getSampleIndex( aTimestamp ), UARTData.UART_TYPE_TXEVENT );
    }
  }
}
