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
package nl.lxtreme.ols.tool.uart.impl;


import static nl.lxtreme.ols.util.NumberUtils.*;

import java.util.logging.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.annotation.*;
import nl.lxtreme.ols.tool.uart.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitOrder;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitEncoding;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitLevel;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialConfiguration;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialDecoderCallback;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;


/**
 * @author jajans
 */
public class UARTAnalyserTask implements ToolTask<UARTDataSet>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( UARTAnalyserTask.class.getName() );

  /**
   * A constant used to distinguish between "real" baudrates and the auto-detect
   * option.
   */
  public static final int AUTO_DETECT_BAUDRATE = -1;

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
  private BitLevel idleLevel;
  private BitEncoding bitEncoding;
  private BitOrder bitOrder;
  private StopBits stopBits;
  private Parity parity;
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
   * @param aBitOrder
   */
  public void setBitOrder( final BitOrder aBitOrder )
  {
    this.bitOrder = aBitOrder;
  }

  /**
   * @param aBitEncoding
   */
  public void setBitEncoding( final BitEncoding aBitEncoding )
  {
    this.bitEncoding = aBitEncoding;
  }

  /**
   * @param aIdleLevel
   */
  public void setIdleLevel( final BitLevel aIdleLevel )
  {
    this.idleLevel = aIdleLevel;
  }

  /**
   * @param aParity
   */
  public void setParity( final Parity aParity )
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
  public void setStopBits( final StopBits aStopBits )
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
   * Emits a new symbol annotation to the interested listener(s).
   * 
   * @param aChannelIndex
   *          the channel index on which the symbol was found;
   * @param aSymbol
   *          the symbol itself;
   * @param aStartSampleIdx
   *          the start sample index of the symbol;
   * @param aEndSampleIdx
   *          the end sample index of the symbol.
   */
  private void addSymbolAnnotation( final int aChannelIndex, final int aSymbol, final long aStartTimestamp,
      final long aEndTimestamp )
  {
    this.annotationListener.onAnnotation( new SampleDataAnnotation( aChannelIndex, aStartTimestamp, aEndTimestamp,
        String.format( "0x%1$X (%1$c)", Integer.valueOf( aSymbol ) ) ) );
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

    final int baudRate;

    if ( this.baudRate == AUTO_DETECT_BAUDRATE )
    {
      // Auto detect the baud rate...
      final int mask = ( 1 << aChannelIndex );
      final BaudRateAnalyzer baudRateAnalyzer = new BaudRateAnalyzer( data.getSampleRate(), data.getValues(), data.getTimestamps(), mask );
      baudRate = baudRateAnalyzer.getBaudRateExact();
      // Set nominal (normalized) baud rate
      aDataSet.setBaudRate( baudRateAnalyzer.getBaudRate() );
    } else {
      baudRate = this.baudRate;
      // Set nominal baud rate
      aDataSet.setBaudRate( baudRate );
    }

    LOG.log( Level.FINE, "Baudrate = {0}bps", Integer.valueOf( baudRate ) );

    if ( baudRate <= 0 )
    {
      LOG.log( Level.INFO, "No (usable) {0}-data found for determining bitlength/baudrate ...",
          aChannelIndex == this.rxdIndex ? UARTDataSet.UART_RXD : UARTDataSet.UART_TXD );
    }
    else
    {

      SerialConfiguration config = new SerialConfiguration( baudRate, this.bitCount,
          this.stopBits, this.parity, this.bitEncoding, this.bitOrder, this.idleLevel );

      AsyncSerialDataDecoder decoder = new AsyncSerialDataDecoder( config, this.context );
      decoder.setProgressListener( this.progressListener );
      decoder.setCallback( new SerialDecoderCallback()
      {
        @Override
        public void onError( final int aChannelIdx, final ErrorType aType, final long aTime )
        {
          final int sampleIdx = data.getSampleIndex( aTime );
          final int eventType = ( aEventType == UARTData.UART_TYPE_RXDATA ) ? UARTData.UART_TYPE_RXEVENT
              : UARTData.UART_TYPE_TXEVENT;

          aDataSet.reportError( aType, aChannelIdx, sampleIdx, eventType );
        }

        @Override
        public void onEvent( final int aChannelIdx, final String aEvent, final long aStartTime, final long aEndTime )
        {
          // Nop
        }

        @Override
        public void onSymbol( final int aChannelIdx, final int aSymbol, final long aStartTime, final long aEndTime )
        {
          final int startSampleIdx = Math.max( data.getSampleIndex( aStartTime ), 0 );
          final int endSampleIdx = Math.min( data.getSampleIndex( aEndTime ), data.getTimestamps().length - 1 );

          aDataSet.reportData( aChannelIndex, startSampleIdx, endSampleIdx, aSymbol, aEventType );

          addSymbolAnnotation( aChannelIndex, aSymbol, aStartTime, aEndTime );
        }
      } );

      final double sampledBitLength = decoder.decodeDataLine( aChannelIndex );
      // Set the actual bit length used, so UARTDataSet can calculate
      // the actual baud rate used.
      aDataSet.setSampledBitLength( sampledBitLength );
    }
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
}
