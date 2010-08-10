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
  public UARTDataSet( final long aStartSampleIdx, final long aStopSampleIdx, final CapturedData aData )
  {
    super( aStartSampleIdx, aStopSampleIdx, aData );

    this.decodedSymbols = 0;
    this.detectedErrors = 0;
  }

  // METHODS

  /**
   * @return
   */
  public int getBitLength()
  {
    return this.bitLength;
  }

  /**
   * @return
   */
  public int getDecodedSymbols()
  {
    return this.decodedSymbols;
  }

  /**
   * @return
   */
  public int getDetectedErrors()
  {
    return this.detectedErrors;
  }

  /**
   * @param aTime
   * @param aName
   */
  public void reportControlHigh( final long aTime, final String aName )
  {
    addData( new UARTData( aTime, aName.toUpperCase() + "_HIGH", indexToTime( aTime ) ) );
  }

  /**
   * @param aTime
   * @param aName
   */
  public void reportControlLow( final long aTime, final String aName )
  {
    addData( new UARTData( aTime, aName.toUpperCase() + "_LOW", indexToTime( aTime ) ) );
  }

  /**
   * @param aTime
   * @param aValue
   * @param aEventType
   */
  public void reportData( final long aTime, final int aValue, final int aEventType )
  {
    addData( new UARTData( aTime, aValue, aEventType, indexToTime( aTime ) ) );
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportFrameError( final long aTime, final int aEventType )
  {
    addData( new UARTData( aTime, "FRAME_ERR", aEventType, indexToTime( aTime ) ) );
    this.detectedErrors++;
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportParityError( final long aTime, final int aEventType )
  {
    addData( new UARTData( aTime, "PARITY_ERR", aEventType, indexToTime( aTime ) ) );
    this.detectedErrors++;
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportStartError( final long aTime, final int aEventType )
  {
    addData( new UARTData( aTime, "START_ERR", aEventType, indexToTime( aTime ) ) );
    this.detectedErrors++;
  }

  /**
   * 
   */
  public void reportSymbol()
  {
    this.decodedSymbols++;
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
