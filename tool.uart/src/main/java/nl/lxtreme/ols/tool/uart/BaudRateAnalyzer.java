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
  // VARIABLES

  private final Frequency<Integer> statData;

  // CONSTRUCTORS

  /**
   * Creates a new BaudRateAnalyzer instance.
   * 
   * @param aValues
   *          the values to determine the baudrate for;
   * @param aTimestamps
   *          the timestamps to use when determining the bit lengths;
   * @param aMask
   *          the value mask to isolate the data.
   */
  public BaudRateAnalyzer( final int[] aValues, final long[] aTimestamps, final int aMask )
  {
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
