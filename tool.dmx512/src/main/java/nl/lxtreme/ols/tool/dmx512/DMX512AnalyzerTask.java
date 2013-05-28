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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.dmx512;


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.annotation.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialConfiguration;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialDecoderCallback;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitOrder;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitEncoding;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitLevel;


/**
 * 
 */
public class DMX512AnalyzerTask implements ToolTask<DMX512DataSet>
{

  // CONSTANTS

  private static final String DMX512_DATA_LABEL = "DMX512 data";

  private static final int BAUDRATE = 250000;
  private static final int DATABITS = 8;
  private static final Parity PARITY = Parity.NONE;
  private static final StopBits STOPBITS = StopBits.TWO;

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final AnnotationListener annotationListener;

  private int dataLine;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DMX512AnalyzerTask} instance.
   */
  public DMX512AnalyzerTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annotationListener = aAnnotationListener;

    this.dataLine = -1;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public DMX512DataSet call() throws Exception
  {
    final AcquisitionResult data = this.context.getData();
    final int[] values = data.getValues();

    int startOfDecode = this.context.getStartSampleIndex();
    final int endOfDecode = this.context.getEndSampleIndex();

    // find first state change on the selected lines
    final int mask = ( 1 << this.dataLine );

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
      throw new IllegalStateException( "No valid data range found for DMX512 analysis!" );
    }

    final DMX512DataSet dataSet = new DMX512DataSet( startOfDecode, endOfDecode, data );

    this.annotationListener.clearAnnotations( this.dataLine );
    this.annotationListener.onAnnotation( new ChannelLabelAnnotation( this.dataLine, DMX512_DATA_LABEL ) );

    final SerialConfiguration config = new SerialConfiguration( BAUDRATE, DATABITS, STOPBITS, PARITY,
        BitEncoding.HIGH_IS_MARK, BitOrder.MSB_FIRST, BitLevel.HIGH);

    final DMX512SerialDataDecoder decoder = new DMX512SerialDataDecoder( config, this.context );
    decoder.setProgressListener( this.progressListener );
    decoder.setCallback( new SerialDecoderCallback()
    {
      /**
       * {@inheritDoc}
       */
      @Override
      public void onError( final int aChannelIdx, final ErrorType aType, final long aTime )
      {
        dataSet.reportError( aChannelIdx, aType, data.getSampleIndex( aTime ) );
      }

      @Override
      public void onEvent( final int aChannelIdx, final String aEvent, final long aStartTime, final long aEndTime )
      {
        dataSet.reportEvent( aChannelIdx, aEvent, data.getSampleIndex( aStartTime ), data.getSampleIndex( aEndTime ) );

        addEventAnnotation( aChannelIdx, aEvent, aStartTime, aEndTime );
      }

      /**
       * {@inheritDoc}
       */
      @Override
      public void onSymbol( final int aChannelIdx, final int aSymbol, final long aStartTime, final long aEndTime )
      {
        dataSet.reportData( aChannelIdx, data.getSampleIndex( aStartTime ), data.getSampleIndex( aEndTime ), aSymbol );

        addSymbolAnnotation( aChannelIdx, aSymbol, aStartTime, aEndTime );
      }

      /**
       * Emits a new symbol annotation to the interested listener(s).
       * 
       * @param aSymbol
       *          the symbol itself;
       * @param aStartSampleIdx
       *          the start sample index of the symbol;
       * @param aEndSampleIdx
       *          the end sample index of the symbol.
       */
      private void addEventAnnotation( final int aChannelIdx, final String aEvent, final long aStartTimestamp,
          final long aEndTimestamp )
      {
        DMX512AnalyzerTask.this.annotationListener.onAnnotation( new SampleDataAnnotation( aChannelIdx,
            aStartTimestamp, aEndTimestamp, aEvent ) );
      }

      /**
       * Emits a new symbol annotation to the interested listener(s).
       * 
       * @param aSymbol
       *          the symbol itself;
       * @param aStartSampleIdx
       *          the start sample index of the symbol;
       * @param aEndSampleIdx
       *          the end sample index of the symbol.
       */
      private void addSymbolAnnotation( final int aChannelIdx, final int aSymbol, final long aStartTimestamp,
          final long aEndTimestamp )
      {
        DMX512AnalyzerTask.this.annotationListener.onAnnotation( new SampleDataAnnotation( aChannelIdx,
            aStartTimestamp, aEndTimestamp, String.format( "0x%1$X (%1$c)", Integer.valueOf( aSymbol ) ) ) );
      }
    } );

    decoder.decodeDataLine( this.dataLine );

    return dataSet;
  }

  /**
   * Returns the channel index of the data line.
   * 
   * @return the data line channel index, >= 0 if set, or -1 if unset.
   */
  public int getDataLine()
  {
    return this.dataLine;
  }

  /**
   * Sets channel index of the data line.
   * 
   * @param aDataLine
   *          the data line channel index to set, >= 0.
   */
  public void setDataLine( final int aDataLine )
  {
    this.dataLine = aDataLine;
  }
}
