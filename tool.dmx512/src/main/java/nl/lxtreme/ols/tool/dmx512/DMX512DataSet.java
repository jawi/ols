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


/**
 * 
 */
public class DMX512DataSet extends BaseDataSet<DMX512Data>
{
  // VARIABLES

  private int decodedSymbols;
  private int detectedErrors;

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
   * @param aChannelIdx
   * @param aStartSampleIdx
   * @param aEndSampleIdx
   * @param aSymbol
   */
  public void reportData( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx, final int aSymbol )
  {
    final int idx = size();
    this.decodedSymbols++;
    addData( new DMX512Data( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, DMX512Data.SYMBOL, aSymbol ) );
  }

  /**
   * @param aChannelIdx
   * @param aSampleIndex
   */
  public void reportFrameError( final int aChannelIdx, final int aSampleIndex )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new DMX512Data( idx, aChannelIdx, aSampleIndex, aSampleIndex + 1, DMX512Data.FRAME_ERROR ) );
  }

  /**
   * @param aChannelIdx
   * @param aSampleIndex
   */
  public void reportParityError( final int aChannelIdx, final int aSampleIndex )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new DMX512Data( idx, aChannelIdx, aSampleIndex, aSampleIndex + 1, DMX512Data.PARITY_ERROR ) );
  }

  /**
   * @param aChannelIdx
   * @param aSampleIndex
   */
  public void reportStartError( final int aChannelIdx, final int aSampleIndex )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new DMX512Data( idx, aChannelIdx, aSampleIndex, aSampleIndex + 1, DMX512Data.START_ERROR ) );
  }
}
