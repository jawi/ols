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


import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.base.*;


/**
 * @author jajans
 */
public final class UARTDataSet extends BaseDataSet<UARTData>
{
  // VARIABLES

  private int decodedSymbols;
  private int bitLength;
  private int detectedErrors;

  // CONSTRUCTORS

  /**
   * Creates a new UARTDataSet instance.
   */
  public UARTDataSet( final int aStartSampleIdx, final int aEndSampleIdx, final CapturedData aData )
  {
    super( aStartSampleIdx, aEndSampleIdx, aData );

    this.decodedSymbols = 0;
    this.detectedErrors = 0;
  }

  // METHODS

  /**
   * Returns the (calculated, exact) baudrate.
   * 
   * @return a baudrate, >= 0.
   */
  public int getBaudRateExact()
  {
    if ( this.bitLength == 0 )
    {
      // Avoid division by zero...
      return 0;
    }
    return getSampleRate() / this.bitLength;
  }

  /**
   * Returns the "normalized" baudrate.
   * 
   * @return a baudrate, >= 0.
   */
  public int getBaudRate()
  {
    int baudRateExact = getBaudRateExact();
    baudRateExact -= ( baudRateExact % 300 );
    return baudRateExact;
  }

  /**
   * Returns the "average" bit length found in the data.
   * 
   * @return an average bit length, >= 0.
   */
  public int getBitLength()
  {
    return this.bitLength;
  }

  /**
   * Returns the number of decoded (data) symbols.
   * 
   * @return a number of decoded (data) symbols, >= 0.
   */
  public int getDecodedSymbols()
  {
    return this.decodedSymbols;
  }

  /**
   * Returns the number of errors.
   * 
   * @return an error count, >= 0.
   */
  public int getDetectedErrors()
  {
    return this.detectedErrors;
  }

  /**
   * @param aTime
   * @param aName
   */
  public void reportControlHigh( final int aChannelIdx, final int aSampleIdx, final String aName )
  {
    final int idx = size();
    addData( new UARTData( idx, aChannelIdx, aSampleIdx, aName.toUpperCase() + "_HIGH" ) );
  }

  /**
   * @param aTime
   * @param aName
   */
  public void reportControlLow( final int aChannelIdx, final int aSampleIdx, final String aName )
  {
    final int idx = size();
    addData( new UARTData( idx, aChannelIdx, aSampleIdx, aName.toUpperCase() + "_LOW" ) );
  }

  /**
   * @param aTime
   * @param aValue
   * @param aEventType
   */
  public void reportData( final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx, final int aValue,
      final int aEventType )
  {
    final int idx = size();
    this.decodedSymbols++;
    addData( new UARTData( idx, aChannelIdx, aStartSampleIdx, aEndSampleIdx, aValue, aEventType ) );
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportFrameError( final int aChannelIdx, final int aSampleIdx, final int aEventType )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new UARTData( idx, aChannelIdx, aSampleIdx, "FRAME_ERR", aEventType ) );
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportParityError( final int aChannelIdx, final int aSampleIdx, final int aEventType )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new UARTData( idx, aChannelIdx, aSampleIdx, "PARITY_ERR", aEventType ) );
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportStartError( final int aChannelIdx, final int aSampleIdx, final int aEventType )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new UARTData( idx, aChannelIdx, aSampleIdx, "START_ERR", aEventType ) );
  }

  /**
   * @param aBitLength
   */
  public void setSampledBitLength( final int aBitLength )
  {
    final int diff = Math.abs( aBitLength - this.bitLength );
    if ( ( this.bitLength > 0 ) && ( diff > 0 ) && ( diff < 50 ) )
    {
      // Take the average as the current and the given bitlength are "close" to
      // each other
      this.bitLength = ( int )( diff / 2.0 );
    }
    this.bitLength = aBitLength;
  }

  /**
   * @see nl.lxtreme.ols.tool.base.BaseDataSet#sort()
   */
  @Override
  public void sort()
  {
    super.sort();
  }
}
