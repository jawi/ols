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


import java.util.logging.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.ErrorType;


/**
 * @author jajans
 */
public final class UARTDataSet extends BaseDataSet<UARTData>
{
  // CONSTANTS

  public static final String UART_RXD = "RxD";
  public static final String UART_TXD = "TxD";
  public static final String UART_CTS = "CTS";
  public static final String UART_RTS = "RTS";
  public static final String UART_DCD = "DCD";
  public static final String UART_RI = "RI";
  public static final String UART_DSR = "DSR";
  public static final String UART_DTR = "DTR";

  private static final Logger LOG = Logger.getLogger( UARTDataSet.class.getName() );

  // VARIABLES

  private int decodedSymbols;
  private double bitLength;
  private int detectedErrors;

  private int baudRate;

  // CONSTRUCTORS

  /**
   * Creates a new UARTDataSet instance.
   */
  public UARTDataSet( final int aStartSampleIdx, final int aEndSampleIdx, final AcquisitionResult aData )
  {
    super( aStartSampleIdx, aEndSampleIdx, aData );

    this.decodedSymbols = 0;
    this.detectedErrors = 0;
    this.bitLength = -1;
  }

  // METHODS

  /**
   * Returns the "normalized" baudrate most people can recognize.
   * 
   * @return a baudrate, >= 0.
   */
  public int getBaudRate()
  {
    return this.baudRate;
  }

  /**
   * Returns the (calculated, exact) baudrate.
   * 
   * @return a baudrate, >= 0.
   */
  public int getBaudRateExact()
  {
    return ( int )( this.getSampleRate() / this.bitLength );
  }

  /**
   * Returns the "average" bit length found in the data.
   * 
   * @return an average bit length, >= 0.
   */
  public double getBitLength()
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
   * Returns whether or not the bitlength is usable.
   * 
   * @return <code>true</code> if a usable bitlength has been decoded,
   *         <code>false</code> otherwise.
   */
  public boolean isBitLengthUsable()
  {
    // TODO where does the 15 come from?!
    return getBitLength() > 15;
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
  public void reportError( final ErrorType aType, final int aChannelIdx, final int aSampleIdx, final int aEventType )
  {
    final int idx = size();
    this.detectedErrors++;
    addData( new UARTData( idx, aChannelIdx, aSampleIdx, aType.name(), aEventType ) );
  }

  /**
   * Sets the baudRate.
   * 
   * @param aBaudRate
   *          the baudRate to set
   */
  public void setBaudRate( final int aBaudRate )
  {
    this.baudRate = aBaudRate;
  }

  /**
   * Sets the (average) bit length for this data set.
   * <p>
   * A bit length is used to determine the baudrate of this data set. If there
   * is already a bit length available from an earlier call to this method, it
   * will be used to average the resulting bit length if both the current and
   * the given bit length are "close". This allows you to use the bit lengths of
   * both the RxD- and TxD-lines to get a good approximation of the actual
   * baudrate.
   * </p>
   * 
   * @param aBitLength
   *          the bit length to set/add, should be >= 0.
   */
  public void setSampledBitLength( final double aBitLength )
  {
    // If the given bit length is "much" smaller (smaller bit length means
    // higher baudrate) than the current one, switch to that one instead; this
    // way we can recover from bad values...
    if ( ( this.bitLength <= 0 ) || ( ( 10.0 * aBitLength ) < this.bitLength ) )
    {
      // First time being called, take the given bit length as our "truth"...
      this.bitLength = aBitLength;
    }
    else
    {
      // Take the average as the current and the given bit lengths are "close"
      // to each other; ignore the given bit length otherwise, as it clobbers
      // our earlier results...
      final double diff = Math.abs( aBitLength - this.bitLength );
      if ( ( diff < 50 ) )
      {
        this.bitLength = ( ( aBitLength + this.bitLength ) / 2.0 );
      }
      else
      {
        LOG.log( Level.INFO, "Ignoring sampled bit length ({0}) as it deviates "
            + "too much from current bit length ({1}).",
            new Object[] { Double.valueOf( aBitLength ), Double.valueOf( this.bitLength ) } );
      }
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.BaseDataSet#sort()
   */
  @Override
  public void sort()
  {
    super.sort();
  }
}
