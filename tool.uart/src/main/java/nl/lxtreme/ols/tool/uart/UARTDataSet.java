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


import java.util.*;


/**
 * @author jajans
 */
public final class UARTDataSet
{
  // VARIABLES

  private final long startOfDecode;
  private final long endOfDecode;
  private final List<UARTData> data;
  private int decodedSymbols;
  private int bitLength;
  private int detectedErrors;

  // CONSTRUCTORS

  /**
   * 
   */
  public UARTDataSet( final long aStartOfDecode, final long aEndOfDecode )
  {
    this.startOfDecode = aStartOfDecode;
    this.endOfDecode = aEndOfDecode;

    this.data = new ArrayList<UARTData>();

    this.decodedSymbols = 0;
    this.detectedErrors = 0;
  }

  // METHODS

  public int getBitLength()
  {
    return this.bitLength;
  }

  /**
   * @return
   */
  public List<UARTData> getDecodedData()
  {
    return this.data;
  }

  public int getDecodedSymbols()
  {
    return this.decodedSymbols;
  }

  public int getDetectedErrors()
  {
    return this.detectedErrors;
  }

  /**
   * @return
   */
  public long getEndOfDecode()
  {
    return this.endOfDecode;
  }

  /**
   * @return
   */
  public long getStartOfDecode()
  {
    return this.startOfDecode;
  }

  /**
   * @return
   */
  public boolean isEmpty()
  {
    return this.data.isEmpty();
  }

  /**
   * @param aIndex
   * @param aName
   */
  public void reportControlHigh( final long aIndex, final String aName )
  {
    this.data.add( new UARTData( aIndex, aName.toUpperCase() + "_HIGH" ) );
  }

  /**
   * @param aIndex
   * @param aName
   */
  public void reportControlLow( final long aIndex, final String aName )
  {
    this.data.add( new UARTData( aIndex, aName.toUpperCase() + "_LOW" ) );
  }

  /**
   * @param aTime
   * @param aValue
   * @param aEventType
   */
  public void reportData( final long aTime, final int aValue, final int aEventType )
  {
    this.data.add( new UARTData( aTime, aValue, aEventType ) );
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportFrameError( final long aTime, final int aEventType )
  {
    this.data.add( new UARTData( aTime, "FRAME_ERR", aEventType ) );
    this.detectedErrors++;
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportParityError( final long aTime, final int aEventType )
  {
    this.data.add( new UARTData( aTime, "PARITY_ERR", aEventType ) );
    this.detectedErrors++;
  }

  /**
   * @param aTime
   * @param aEventType
   */
  public void reportStartError( final long aTime, final int aEventType )
  {
    this.data.add( new UARTData( aTime, "START_ERR", aEventType ) );
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
   * 
   */
  public void sort()
  {
    Collections.sort( this.data );
  }
}
