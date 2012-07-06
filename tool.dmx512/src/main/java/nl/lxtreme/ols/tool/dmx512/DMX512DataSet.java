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
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;


/**
 * 
 */
public class DMX512DataSet extends BaseDataSet<DMX512Data>
{
  // CONSTANTS

  /**
   * The space-before-break used as preamble for the actual data frame.
   */
  public static final String EVENT_SBB = "Start before break";
  /**
   * The mark-after-break used as preamble for the actual data frame.
   */
  public static final String EVENT_MAB = "Mark after break";

  // VARIABLES

  private int decodedSymbols;
  private int detectedErrors;
  private int symbolsBetweenMaB;
  private Boolean inMaB = Boolean.FALSE;

  // CONSTRUCTORS

  /**
   * Creates a new DMX512DataSet instance.
   * 
   * @param aStartOfDecodeIdx
   * @param aEndOfDecodeIdx
   * @param aData
   */
  public DMX512DataSet( final int aStartOfDecodeIdx, final int aEndOfDecodeIdx, final AcquisitionResult aData )
  {
    super( aStartOfDecodeIdx, aEndOfDecodeIdx, aData );
  }

  // METHODS

  /**
   * Returns the current value of decodedSymbols.
   * 
   * @return the decodedSymbols
   */
  public int getDecodedSymbols()
  {
    return this.decodedSymbols;
  }

  /**
   * Returns the current value of detectedErrors.
   * 
   * @return the detectedErrors
   */
  public int getDetectedErrors()
  {
    return this.detectedErrors;
  }

  /**
   * @return the number of slots in a DMX512-packet, or -1 if this could not be
   *         determined.
   */
  public int getSlotCount()
  {
    return this.symbolsBetweenMaB - 1;
  }

  /**
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   * @param aSymbol
   */
  public void reportData( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx, final int aSymbol )
  {
    final int idx = size();
    this.decodedSymbols++;
    if ( Boolean.TRUE.equals( this.inMaB ) )
    {
      this.symbolsBetweenMaB++;
    }
    addData( new DMX512Data( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aSymbol ) );
  }

  /**
   * @param aChannelIdx
   * @param aSampleIndex
   */
  public void reportError( final int aChannelIdx, final ErrorType aType, final int aSampleIndex )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new DMX512Data( idx, aChannelIdx, aSampleIndex, aType ) );
  }

  /**
   * @param aChannelIdx
   * @param aEvent
   * @param aSampleIndex
   * @param aSampleIndex2
   */
  public void reportEvent( final int aChannelIdx, final String aEvent, final int aStartSampleIdx,
      final int aEndSampleIdx )
  {
    final int idx = size();
    if ( EVENT_MAB.equals( aEvent ) )
    {
      if ( Boolean.FALSE.equals( this.inMaB ) )
      {
        this.inMaB = Boolean.TRUE;
      }
      else if ( Boolean.TRUE.equals( this.inMaB ) )
      {
        this.inMaB = null;
      }
    }
    addData( new DMX512Data( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aEvent ) );
  }
}
