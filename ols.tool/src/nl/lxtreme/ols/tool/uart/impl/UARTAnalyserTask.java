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


import static nl.lxtreme.ols.tool.api.AnnotationHelper.*;
import static nl.lxtreme.ols.tool.base.NumberUtils.*;

import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import aQute.bnd.annotation.metatype.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.uart.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialConfiguration;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialDecoderCallback;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;


/**
 * Provides a UART analyzer task.
 */
public class UARTAnalyserTask implements Callable<Void>
{
  // INNER TYPES

  /**
   * Provides a custom annotation to show the baudrate information.
   */
  public static class BaudrateAnnotation implements DataAnnotation
  {
    // VARIABLES

    private final int channelIdx;
    private final Boolean data;
    private final Map<String, Object> properties;

    /**
     * Creates a new {@link BaudrateAnnotation} instance.
     */
    public BaudrateAnnotation( final int aChannelIdx, final BaudRateAnalyzer aAnalyzer )
    {
      this.channelIdx = aChannelIdx;
      // TODO where does the 15 come from?!
      this.data = Boolean.valueOf( aAnalyzer.getBestBitLength() > 15 );
      this.properties = new HashMap<String, Object>( 3 );
      this.properties.put( "bitlength", Integer.valueOf( aAnalyzer.getBestBitLength() ) );
      this.properties.put( "baudrate", Integer.valueOf( aAnalyzer.getBaudRate() ) );
      this.properties.put( "baudrateExact", Integer.valueOf( aAnalyzer.getBaudRateExact() ) );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getData()
    {
      return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getChannelIndex()
    {
      return this.channelIdx;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final Annotation aOther )
    {
      // will cause this annotation to be one of the first ones...
      return -1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> getProperties()
    {
      return this.properties;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getEndTimestamp()
    {
      return 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getStartTimestamp()
    {
      return 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      StringBuilder sb = new StringBuilder();
      int bitlength = ( ( Integer )this.properties.get( "bitlength" ) ).intValue();
      if ( bitlength <= 0 )
      {
        sb.append( "Baud rate calculation failed!" );
      }
      else
      {
        sb.append( "Baudrate = " ).append( this.properties.get( "baudrate" ) );
        sb.append( " (exact = " ).append( this.properties.get( "baudrateExact" ) ).append( ")" );
        if ( Boolean.FALSE.equals( this.data ) )
        {
          sb.append( '\n' ).append( "The baudrate may be wrong, use a higher samplerate to avoid this!" );
        }
      }
      sb.append( '\n' );
      return sb.toString();
    }
  }

  // CONSTANTS

  static final String KEY_EVENT_TYPE = "eventType";

  private static final String UART_RXD = "RxD";
  private static final String UART_TXD = "TxD";
  private static final String UART_CTS = "CTS";
  private static final String UART_RTS = "RTS";
  private static final String UART_DCD = "DCD";
  private static final String UART_RI = "RI";
  private static final String UART_DSR = "DSR";
  private static final String UART_DTR = "DTR";

  private static final int UART_TYPE_RXDATA = 3;
  private static final int UART_TYPE_TXDATA = 4;

  private static final Logger LOG = Logger.getLogger( UARTAnalyserTask.class.getName() );

  /**
   * A constant used to distinguish between "real" baudrates and the auto-detect
   * option.
   */
  public static final int AUTO_DETECT_BAUDRATE = -1;

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;

  private final int rxdIndex;
  private final int txdIndex;
  private final int ctsIndex;
  private final int rtsIndex;
  private final int dcdIndex;
  private final int riIndex;
  private final int dsrIndex;
  private final int dtrIndex;
  private final boolean inverted;
  private final boolean msbFirst;
  private final StopBits stopBits;
  private final Parity parity;
  private final int bitCount;
  private final int baudRate;

  // CONSTRUCTORS

  /**
   * @param aContext
   * @param aAnnotationListener
   */
  public UARTAnalyserTask( final ToolContext aContext, final Configuration aConfiguration )
  {
    this.context = aContext;
    this.progressListener = aContext.getProgressListener();

    UARTConfig config = Configurable.createConfigurable( UARTConfig.class, aConfiguration.asMap() );

    this.rxdIndex = config.rxdIdx();
    this.txdIndex = config.txdIdx();
    this.ctsIndex = config.ctsIdx();
    this.rtsIndex = config.rtsIdx();
    this.dcdIndex = config.dcdIdx();
    this.riIndex = config.riIdx();
    this.dsrIndex = config.dsrIdx();
    this.dtrIndex = config.dtrIdx();
    this.baudRate = config.baudrate();
    this.stopBits = config.stopBits();
    this.parity = config.parity();
    this.bitCount = config.bitCount();
    this.inverted = config.inverted();
    this.msbFirst = config.msbFirst();
  }

  // METHODS

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public Void call() throws ToolException
  {
    final AcquisitionData data = this.context.getAcquisitionData();

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

    // decode RxD/TxD data lines...
    if ( this.rxdIndex >= 0 )
    {
      decodeData( this.rxdIndex, UART_RXD, UART_TYPE_RXDATA, startOfDecode, endOfDecode );
    }
    if ( this.txdIndex >= 0 )
    {
      decodeData( this.txdIndex, UART_TXD, UART_TYPE_TXDATA, startOfDecode, endOfDecode );
    }

    // decode control lines...
    if ( this.ctsIndex >= 0 )
    {
      decodeControl( this.ctsIndex, UART_CTS, startOfDecode, endOfDecode );
    }
    if ( this.rtsIndex >= 0 )
    {
      decodeControl( this.rtsIndex, UART_RTS, startOfDecode, endOfDecode );
    }
    if ( this.dcdIndex >= 0 )
    {
      decodeControl( this.dcdIndex, UART_DCD, startOfDecode, endOfDecode );
    }
    if ( this.riIndex >= 0 )
    {
      decodeControl( this.riIndex, UART_RI, startOfDecode, endOfDecode );
    }
    if ( this.dsrIndex >= 0 )
    {
      decodeControl( this.dsrIndex, UART_DSR, startOfDecode, endOfDecode );
    }
    if ( this.dtrIndex >= 0 )
    {
      decodeControl( this.dtrIndex, UART_DTR, startOfDecode, endOfDecode );
    }

    return null;
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
  private BaudRateAnalyzer createBaudRateAnalyzer( final AcquisitionData aData, final int aMask )
  {
    if ( this.baudRate == AUTO_DETECT_BAUDRATE )
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
  private void decodeControl( final int aChannelIndex, final String aName, final int aStartSampleIdx,
      final int aEndSampleIdx )
  {
    final AcquisitionData data = this.context.getAcquisitionData();
    final AnnotationHelper annotationHelper = new AnnotationHelper( this.context );

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, "Decoding control: {0} ...", aName );
    }

    annotationHelper.clearAnnotations( aChannelIndex );
    annotationHelper.addAnnotation( aChannelIndex, aName );

    final int mask = ( 1 << aChannelIndex );

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    this.progressListener.setProgress( 0 );

    int oldValue = values[aStartSampleIdx] & mask;
    for ( int i = aStartSampleIdx + 1; i < aEndSampleIdx; i++ )
    {
      final int value = values[i] & mask;
      final long startTime = timestamps[i];
      final long endTime = startTime + 1;

      final Edge edge = Edge.toEdge( oldValue, value );
      if ( edge.isRising() )
      {
        annotationHelper.addAnnotation( aChannelIndex, startTime, endTime, aName + " High", KEY_COLOR, "#e0e0e0" );
      }
      if ( edge.isFalling() )
      {
        annotationHelper.addAnnotation( aChannelIndex, startTime, endTime, aName + " Low", KEY_COLOR, "#e0e0e0" );
      }
      oldValue = value;

      // update progress
      this.progressListener.setProgress( getPercentage( i, aStartSampleIdx, aEndSampleIdx ) );
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
  private void decodeData( final int aChannelIndex, final String aName, final int aEventType,
      final int aStartSampleIdx, final int aEndSampleIdx )
  {
    final AcquisitionData data = this.context.getAcquisitionData();
    final AnnotationHelper annotationHelper = new AnnotationHelper( this.context );

    annotationHelper.clearAnnotations( aChannelIndex );
    annotationHelper.addAnnotation( aChannelIndex, aName );

    final int mask = ( 1 << aChannelIndex );
    final BaudRateAnalyzer baudrateAnalyzer = createBaudRateAnalyzer( data, mask );

    final int bitLength = baudrateAnalyzer.getBestBitLength();

    LOG.log( Level.FINE, "Baudrate = {0}bps", Integer.valueOf( bitLength ) );

    if ( bitLength <= 0 )
    {
      LOG.log( Level.INFO, "No (usable) {0}-data found for determining bitlength/baudrate ...",
          aChannelIndex == this.rxdIndex ? UART_RXD : UART_TXD );
    }
    else
    {
      // We know the avg. bitlength, so we can use it for calculating the
      // baudrate...
      if ( LOG.isLoggable( Level.FINE ) )
      {
        LOG.fine( "Samplerate: " + data.getSampleRate() + ", bitlength: " + bitLength + ", baudrate = "
            + baudrateAnalyzer.getBaudRate() );
      }

      this.context.addAnnotation( new BaudrateAnnotation( aChannelIndex, baudrateAnalyzer ) );

      SerialConfiguration config = new SerialConfiguration( baudrateAnalyzer.getBaudRateExact(), this.bitCount,
          this.stopBits, this.parity, this.inverted, this.msbFirst );

      AsyncSerialDataDecoder decoder = new AsyncSerialDataDecoder( config, this.context );
      decoder.setProgressListener( this.progressListener );
      decoder.setCallback( new SerialDecoderCallback()
      {
        @Override
        public void onError( final int aChannelIdx, final ErrorType aType, final long aTime )
        {
          switch ( aType )
          {
            case FRAME:
              annotationHelper.addAnnotation( aChannelIdx, aTime, aTime + 1, "Frame error", KEY_ERROR, Boolean.TRUE,
                  KEY_COLOR, "#ff6600", KEY_EVENT_TYPE, Integer.valueOf( aEventType ) );
              break;

            case PARITY:
              annotationHelper.addAnnotation( aChannelIdx, aTime, aTime + 1, "Parity error", KEY_ERROR, Boolean.TRUE,
                  KEY_COLOR, "#ff9900", KEY_EVENT_TYPE, Integer.valueOf( aEventType ) );
              break;

            case START:
              annotationHelper.addAnnotation( aChannelIdx, aTime, aTime + 1, "Start error", KEY_ERROR, Boolean.TRUE,
                  KEY_COLOR, "#ffcc00", KEY_EVENT_TYPE, Integer.valueOf( aEventType ) );
              break;
          }
        }

        @Override
        public void onEvent( final int aChannelIdx, final String aEvent, final long aStartTime, final long aEndTime )
        {
          // Nop
        }

        @Override
        public void onSymbol( final int aChannelIdx, final int aSymbol, final long aStartTime, final long aEndTime )
        {
          annotationHelper.addSymbolAnnotation( aChannelIdx, aStartTime, aEndTime, aSymbol );
        }
      } );
      decoder.decodeDataLine( aChannelIndex );
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
}
