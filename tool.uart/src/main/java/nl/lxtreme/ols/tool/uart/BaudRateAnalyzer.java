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
package nl.lxtreme.ols.tool.uart;


import nl.lxtreme.ols.util.analysis.*;


/**
 * Inner class for statistical baudrate analysis. Creates a histogram that
 * allows to evaluate each detected bit length. The bit length with the highest
 * occurrence is used for baudrate calculation.
 */
final class BaudRateAnalyzer
{
  // CONSTANTS

  private static final int[] COMMON_BAUDRATES = { 150, 300, 600, 1200, 2400, 4800, 9600, 19200, 28800, 38400, 57600,
      115200, 230400, 460800, 921600 };

  // VARIABLES

  private final double sampleRate;
  private final Frequency<Integer> statData;

  // CONSTRUCTORS

  /**
   * Creates a new BaudRateAnalyzer instance.
   * 
   * @param aSampleRate
   *          the sample rate at which the incoming data was sampled;
   * @param aValues
   *          the values to determine the baudrate for;
   * @param aTimestamps
   *          the timestamps to use when determining the bit lengths;
   * @param aMask
   *          the value mask to isolate the data.
   */
  public BaudRateAnalyzer( final int aSampleRate, final int[] aValues, final long[] aTimestamps, final int aMask )
  {
    this.sampleRate = aSampleRate;
    this.statData = new Frequency<Integer>();

    long lastTransition = 0;
    int lastBitValue = aValues[0] & aMask;

    for ( int i = 0; i < aValues.length; i++ )
    {
      final int bitValue = aValues[i] & aMask;

      if ( lastBitValue != bitValue )
      {
        final int bitLength = ( int )( aTimestamps[i] - lastTransition );
        this.statData.addValue( Integer.valueOf( bitLength ) );

        lastTransition = aTimestamps[i];
      }

      lastBitValue = aValues[i] & aMask;
    }
  }

  // METHODS

  /**
   * Returns the "normalized" baudrate most people can recognize.
   * 
   * @return a baudrate, >= 150 if a valid baudrate could be determined, or -1
   *         if no valid baudrate could be determined.
   */
  public int getBaudRate()
  {
    final int br = getBaudRateExact();

    int baudRateRounded = -1;
    // Try to find the common baudrate that belongs to the exact one...
    for ( int idx = 1; ( baudRateRounded < 0 ) && ( idx < COMMON_BAUDRATES.length ); idx++ )
    {
      int delta = ( COMMON_BAUDRATES[idx] - COMMON_BAUDRATES[idx - 1] ) / 2;
      if ( ( br >= ( COMMON_BAUDRATES[idx] - delta ) ) && ( br <= ( COMMON_BAUDRATES[idx] + delta ) ) )
      {
        baudRateRounded = COMMON_BAUDRATES[idx];
      }
    }

    return baudRateRounded;
  }

  /**
   * Returns the calculated baudrate.
   * 
   * @return a baudrate, calculated by dividing the samplerate by the "best" bit
   *         length, as returned by {@link #getBestBitLength()}. Returns -1 if
   *         no best bit length could be determined.
   */
  public int getBaudRateExact()
  {
    final int bestBitLength = getBestBitLength();
    if ( bestBitLength < 0 )
    {
      return -1;
    }
    return ( int )( this.sampleRate / bestBitLength );
  }

  /**
   * Returns the highest ranked bit length (= with the highest count).
   * 
   * @return the best bit length, >= 0 or -1 if there is not best bit length.
   */
  public int getBestBitLength()
  {
    final Integer highestRanked = this.statData.getHighestRanked();
    return highestRanked == null ? -1 : highestRanked.intValue();
  }
}
