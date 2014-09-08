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
 * Utility for statistical baudrate analysis. Creates a histogram that allows to
 * evaluate each detected bit length. The bit length with the highest occurrence
 * is used for baudrate calculation.
 */
public final class BaudRateAnalyzer
{
  // VARIABLES

  private final double sampleRate;
  private final Frequency<Integer> statData;

  // CONSTRUCTORS

  /**
   * Creates a new {@link BaudRateAnalyzer} instance.
   * 
   * @param aSampleRate
   *          the sample rate at which the incoming data was sampled;
   * @param aFixedBaudRate
   *          the fixed baud rate that this analyzer should return.
   */
  public BaudRateAnalyzer( final int aSampleRate, final int aFixedBaudRate )
  {
    this.sampleRate = aSampleRate;
    this.statData = new Frequency<Integer>();

    // We already know our baudrate, so lets put a single value for the
    // corresponding bitlength in our frequency mapping to let it be used...
    final int bitLength = ( int )Math.round( aSampleRate / ( double )aFixedBaudRate );
    this.statData.addValue( Integer.valueOf( bitLength ) );
  }

  /**
   * Creates a new {@link BaudRateAnalyzer} instance.
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
   * @return a baudrate, >= 150 if a "common" baudrate could be determined, or
   *         the exact baudrate if no "common" baudrate could be determined.
   * @see #getBaudRateExact()
   */
  public int getBaudRate()
  {
    final int br = getBaudRateExact();
    final int[] commonBaudrates = AsyncSerialDataDecoder.COMMON_BAUDRATES;

    int baudRateRounded = -1;
    // Try to find the common baudrate that belongs to the exact one...
    for ( int idx = 1; ( baudRateRounded < 0 ) && ( idx < commonBaudrates.length ); idx++ )
    {
      int delta = ( commonBaudrates[idx] - commonBaudrates[idx - 1] ) / 2;
      if ( ( br >= ( commonBaudrates[idx] - delta ) ) && ( br <= ( commonBaudrates[idx] + delta ) ) )
      {
        baudRateRounded = commonBaudrates[idx];
      }
    }

    if ( baudRateRounded < 0 )
    {
      return br;
    }

    return baudRateRounded;
  }

  /**
   * Returns the calculated baudrate, as exact value.
   * 
   * @return a baudrate, calculated by dividing the sample rate by the "best"
   *         bit length, as returned by {@link #getBestBitLength()}. Returns -1
   *         if no best bit length could be determined.
   */
  public int getBaudRateExact()
  {
    final double bestBitLength = getBestBitLength();
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
  public double getBestBitLength()
  {
    // Assume that the one-bit transitions are the most frequent.
    final Integer highestRanked = this.statData.getHighestRanked();
    long sum = 0, count = 0;

    if ( highestRanked != null )
    {
      double min = highestRanked.doubleValue() * 0.75;
      double max = highestRanked.doubleValue() * 1.25;

      for ( final Integer length : this.statData.values() )
      {
        double bitlength = length.doubleValue();
        if ( min < bitlength && bitlength < max )
        {
          final long rank = this.statData.getCount( length );
          sum += bitlength * rank;
          count += rank;
        }
      }
    }

    // Return the average of all bit lengths near the most frequent one
    return ( highestRanked == null ) ? -1 : ( ( ( double )sum ) / count );
  }
}
