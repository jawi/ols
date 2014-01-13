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

/**
 * Represents a small DTO for measured pulse count information.
 */
final class PulseCountInfo
{
  final Double measureTime;
  final Integer risingEdgeCount;
  final Integer fallingEdgeCount;
  final Integer totalEdgeCount;
  final long totalLowTime;
  final long totalHighTime;
  final Integer pulseCount;
  final int sampleRate;
  final boolean hasTimingData;

  /**
   * Creates a new {@link PulseCountInfo} instance.
   */
  public PulseCountInfo( final double aMeasureTime, final int aRisingEdgeCount, final int aFallingEdgeCount,
      final long aTotalLowTime, final long aTotalHighTime, final int aSampleRate, final boolean aHasTimingData )
  {
    this.measureTime = Double.valueOf( aMeasureTime );
    this.risingEdgeCount = Integer.valueOf( aRisingEdgeCount );
    this.fallingEdgeCount = Integer.valueOf( aFallingEdgeCount );
    this.totalEdgeCount = Integer.valueOf( aRisingEdgeCount + aFallingEdgeCount );
    this.totalLowTime = aTotalLowTime;
    this.totalHighTime = aTotalHighTime;
    this.pulseCount = Integer.valueOf( this.totalEdgeCount.intValue() / 2 );
    this.sampleRate = aSampleRate;
    this.hasTimingData = aHasTimingData;
  }

  /**
   * @return
   */
  public double getDutyCycle()
  {
    final double avgHighTime = getAveragePulseHighTime();
    final double avgLowTime = getAveragePulseLowTime();
    return 100.0 * ( avgHighTime / ( avgHighTime + avgLowTime ) );
  }

  /**
   * @return
   */
  public Double getFrequency()
  {
    return Double.valueOf( this.sampleRate / ( getAveragePulseHighTime() + getAveragePulseLowTime() ) );
  }

  /**
   * @return
   */
  private double getAveragePulseHighTime()
  {
    return ( this.totalHighTime / this.fallingEdgeCount.doubleValue() );
  }

  /**
   * @return
   */
  private double getAveragePulseLowTime()
  {
    return ( this.totalLowTime / this.risingEdgeCount.doubleValue() );
  }
}