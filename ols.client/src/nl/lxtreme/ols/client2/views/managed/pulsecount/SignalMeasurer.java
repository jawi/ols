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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.pulsecount;


import nl.lxtreme.ols.common.acquisition.*;


/**
 * Does the actual measurement of the signal.
 */
final class SignalMeasurer
{
  // VARIABLES

  private final AcquisitionData result;
  private final int mask;
  private final long startTimestamp;
  private final long endTimestamp;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SignalMeasurer} instance.
   */
  public SignalMeasurer( final AcquisitionData aResult, final int aIndex, final long aStartTimestamp,
      final long aEndTimestamp )
  {
    this.result = aResult;
    this.mask = ( 1 << aIndex );
    this.startTimestamp = aStartTimestamp;
    this.endTimestamp = aEndTimestamp;
  }

  // METHODS

  /**
   * Executes the actual measurement.
   * 
   * @return the measurement information, never <code>null</code>.
   */
  public PulseCountInfo run()
  {
    int startIdx = this.result.getSampleIndex( this.startTimestamp );
    int endIdx = this.result.getSampleIndex( this.endTimestamp );

    boolean hasTimingData = this.result.hasTimingData();

    int[] values = this.result.getValues();
    long[] timestamps = this.result.getTimestamps();

    int fallingEdgeCount = 0;
    long highTime = 0;
    int risingEdgeCount = 0;
    long lowTime = 0;

    int i = startIdx;
    long lastTransition = timestamps[i];
    int lastBitValue = values[i++] & this.mask;

    for ( ; !Thread.currentThread().isInterrupted() && ( i <= endIdx ); i++ )
    {
      int bitValue = values[i] & this.mask;
      Edge edge = Edge.toEdge( lastBitValue, bitValue );

      if ( !edge.isNone() )
      {
        long periodTime = timestamps[i] - lastTransition;
        lastTransition = timestamps[i];

        if ( edge.isRising() )
        {
          // Low to high transition: previously seen a low-state...
          risingEdgeCount++;
          lowTime += periodTime;
        }
        else
        /* if ( edge.isFalling() ) */
        {
          // High to low transition: previously seen a high-state...
          fallingEdgeCount++;
          highTime += periodTime;
        }
      }

      lastBitValue = bitValue;
    }

    double measureTime = Math.abs( ( this.endTimestamp - this.startTimestamp ) / ( double )this.result.getSampleRate() );

    return new PulseCountInfo( measureTime, risingEdgeCount, fallingEdgeCount, lowTime, highTime,
        this.result.getSampleRate(), hasTimingData );
  }
}
