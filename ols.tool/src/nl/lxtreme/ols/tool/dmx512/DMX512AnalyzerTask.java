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


import static nl.lxtreme.ols.common.annotation.DataAnnotation.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitEncoding;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitLevel;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.BitOrder;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialConfiguration;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.SerialDecoderCallback;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;


/**
 * Provides the actual DMX512 analyzer.
 */
public class DMX512AnalyzerTask implements ToolTask<DMX512DataSet>
{
  // CONSTANTS

  /**
   * The space-before-break used as preamble for the actual data frame.
   */
  static final String EVENT_SBB = "Start before break";
  /**
   * The mark-after-break used as preamble for the actual data frame.
   */
  static final String EVENT_MAB = "Mark after break";
  /**
   * Used to denote a complete DMX512 packet (a complete series of slots between
   * two mark after breaks.
   */
  static final String EVENT_PACKET = "Packet";
  /**
   * The property key to denote the number of slots in a packet.
   */
  static final String KEY_SLOT_COUNT = "count";

  private static final String DMX512_DATA_LABEL = "DMX512 data";

  private static final int BAUDRATE = 250000;
  private static final int DATABITS = 8;
  private static final Parity PARITY = Parity.NONE;
  private static final StopBits STOPBITS = StopBits.TWO;

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;

  private int dataLine;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DMX512AnalyzerTask} instance.
   */
  public DMX512AnalyzerTask( final ToolContext aContext, final ToolProgressListener aProgressListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;

    this.dataLine = -1;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public DMX512DataSet call() throws Exception
  {
    final AcquisitionData data = this.context.getData();
    final int[] values = data.getValues();
    final ToolAnnotationHelper annotationHelper = new ToolAnnotationHelper( this.context );

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

    annotationHelper.clearAnnotations( this.dataLine );
    annotationHelper.addLabelAnnotation( this.dataLine, DMX512_DATA_LABEL );

    final SerialConfiguration config = new SerialConfiguration( BAUDRATE, DATABITS, STOPBITS, PARITY,
        BitEncoding.HIGH_IS_MARK, BitOrder.MSB_FIRST, BitLevel.HIGH );

    final DMX512SerialDataDecoder decoder = new DMX512SerialDataDecoder( config, this.context );
    decoder.setProgressListener( this.progressListener );
    decoder.setCallback( new SerialDecoderCallback()
    {
      private Boolean inMaB = Boolean.FALSE;
      private int symbolsBetweenMaB = 0;
      private long startTimeMaB = 0;

      /**
       * {@inheritDoc}
       */
      @Override
      public void onError( final int aChannelIdx, final ErrorType aType, final long aTime )
      {
        switch ( aType )
        {
          case FRAME:
            annotationHelper.addErrorAnnotation( aChannelIdx, aTime, aTime + 1, "Frame error", KEY_COLOR, "#ff6600" );
            break;

          case PARITY:
            annotationHelper.addErrorAnnotation( aChannelIdx, aTime, aTime + 1, "Parity error", KEY_COLOR, "#ff9900" );
            break;

          case START:
            annotationHelper.addErrorAnnotation( aChannelIdx, aTime, aTime + 1, "Start error", KEY_COLOR, "#ffcc00" );
            break;
        }

        dataSet.reportError( aChannelIdx, aType, data.getSampleIndex( aTime ) );
      }

      @Override
      public void onEvent( final int aChannelIdx, final String aEvent, final long aStartTime, final long aEndTime )
      {
        annotationHelper.addEventAnnotation( aChannelIdx, aStartTime, aEndTime, aEvent );

        if ( EVENT_MAB.equals( aEvent ) )
        {
          if ( Boolean.FALSE.equals( this.inMaB ) )
          {
            this.startTimeMaB = aStartTime;
            this.inMaB = Boolean.TRUE;
          }
          else if ( Boolean.TRUE.equals( this.inMaB ) )
          {
            // Emit an additional annotation to denote the slot length...
            Integer slotCount = Integer.valueOf( this.symbolsBetweenMaB - 1 );
            if ( slotCount.intValue() > 0 )
            {
              annotationHelper.addEventAnnotation( aChannelIdx, this.startTimeMaB, aEndTime, EVENT_PACKET,
                  KEY_SLOT_COUNT, slotCount, KEY_DESCRIPTION, String.format( "%d slots", slotCount ) );
            }

            this.inMaB = Boolean.FALSE;
            this.symbolsBetweenMaB = 0;
          }
        }

        dataSet.reportEvent( aChannelIdx, aEvent, data.getSampleIndex( aStartTime ), data.getSampleIndex( aEndTime ) );
      }

      /**
       * {@inheritDoc}
       */
      @Override
      public void onSymbol( final int aChannelIdx, final int aSymbol, final long aStartTime, final long aEndTime )
      {
        if ( Boolean.TRUE.equals( this.inMaB ) )
        {
          this.symbolsBetweenMaB++;
        }

        annotationHelper.addSymbolAnnotation( aChannelIdx, aStartTime, aEndTime, aSymbol );

        dataSet.reportData( aChannelIdx, data.getSampleIndex( aStartTime ), data.getSampleIndex( aEndTime ), aSymbol );
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
