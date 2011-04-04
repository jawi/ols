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
package nl.lxtreme.ols.tool.measure;


import java.util.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.analysis.*;


/**
 * @author jajans
 */
public class ClockFrequencyMeasureWorker extends BaseAsyncToolWorker<ClockFrequencyMeasureWorker.ClockStats>
{
  // INNER TYPES

  public static class ClockStats
  {
    private final long rising;
    private final long falling;
    private final double period;
    private final double frequency;

    /**
     * 
     */
    ClockStats( final long aBestRising, final long aBestFalling, final long aSampleRate )
    {
      this.rising = aBestRising;
      this.falling = aBestFalling;

      this.period = this.rising + this.falling;
      this.frequency = aSampleRate / this.period;
    }

    /**
     * @return
     */
    public double getDutyCycle()
    {
      if ( this.period == 0.0 )
      {
        return 0.0;
      }
      return this.rising / this.period;
    }

    /**
     * @return
     */
    public String getDutyCycleDisplayText()
    {
      final double dutycycle = getDutyCycle();
      if ( dutycycle == 0.0 )
      {
        return MeasurementDialog.EMPTY_TEXT;
      }
      return String.format( "%.1f%%", Double.valueOf( getDutyCycle() * 100.0 ) );
    }

    /**
     * @return the frequency
     */
    public double getFrequency()
    {
      return this.frequency;
    }

    /**
     * @return
     */
    public String getFrequencyDisplayText()
    {
      if ( this.period == 0.0 )
      {
        return MeasurementDialog.EMPTY_TEXT;
      }
      return DisplayUtils.displayFrequency( this.frequency );
    }
  }

  // VARIABLES

  private final int channelMask;
  private final Map<Edge, Frequency<Long>> stats;

  // CONSTRUCTORS

  /**
   * Creates a new ClockFrequencyMeasureWorker instance.
   * 
   * @param aData
   *          the data to work upon;
   * @param aChannel
   *          the channel to measure the clock on;
   * @param aStartIndex
   *          the starting index (cursor A);
   * @param aEndIndex
   *          the ending index (cursor B).
   */
  public ClockFrequencyMeasureWorker( final DataContainer aData, final ToolContext aContext, final int aChannel )
  {
    super( aData, aContext );

    this.channelMask = ( 1 << aChannel );

    this.stats = new HashMap<Edge, Frequency<Long>>( 2 );
    // Populate with empty distribution maps...
    this.stats.put( Edge.RISING, new Frequency<Long>() );
    this.stats.put( Edge.FALLING, new Frequency<Long>() );
    this.stats.put( Edge.NONE, new Frequency<Long>() );
  }

  // METHODS

  /**
   * @param aFrequencyDistribution
   * @return
   */
  private static double getMean( final Frequency<Long> aFrequencyDistribution )
  {
    double mean = 0;
    for ( Long value : aFrequencyDistribution.values() )
    {
      if ( value != null )
      {
        mean += value.longValue();
      }
    }
    mean /= aFrequencyDistribution.getUniqueValueCount();
    return mean;
  }

  /**
   * @param aFrequencyDistribution
   * @return
   */
  private static double getVariance( final Frequency<Long> aFrequencyDistribution, final double aMean )
  {
    double variance = 0;
    for ( Long value : aFrequencyDistribution.values() )
    {
      if ( value != null )
      {
        final double d = value.doubleValue() - aMean;
        variance += ( d * d );
      }
    }
    variance /= aFrequencyDistribution.getUniqueValueCount();
    return variance;
  }

  /**
   * @param aValue
   * @param aDefault
   * @return
   */
  private static long nvl( final Long aValue, final long aDefault )
  {
    if ( aValue == null )
    {
      return aDefault;
    }
    return aValue.longValue();
  }

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected ClockStats doInBackground() throws Exception
  {
    final int[] values = getValues();
    final long[] timestamps = getTimestamps();

    final ToolContext context = getContext();
    int start = context.getStartSampleIndex();
    int end = context.getEndSampleIndex();

    int i = Math.max( 0, start - 1 );
    long lastTransition = 0;
    int lastBitValue = values[i] & this.channelMask;

    long prevTime = -1L;
    Edge edge = null;

    final double sr = getSampleRate();

    for ( ; i < end; i++ )
    {
      final int bitValue = values[i] & this.channelMask;

      if ( lastBitValue != bitValue )
      {
        if ( lastBitValue > bitValue )
        {
          // rising edge
          edge = Edge.RISING;
        }
        else
        {
          // falling edge
          edge = Edge.FALLING;
        }

        final long time = timestamps[i] - lastTransition;

        final Frequency<Long> edgeStats = this.stats.get( edge );
        edgeStats.addValue( Long.valueOf( time ) );

        if ( prevTime >= 0 )
        {
          long diff = time + prevTime;
          System.out.printf( "p = %f, dc = %.3f\n", ( sr / diff ), ( ( time / ( double )diff ) * 100.0 ) );
          this.stats.get( Edge.NONE ).addValue( Long.valueOf( diff ) );
          prevTime = -1L;
        }
        else
        {
          prevTime = time;
        }

        lastTransition = timestamps[i];
      }

      lastBitValue = bitValue;
    }

    final Frequency<Long> risingEdgeStats = this.stats.get( Edge.RISING );
    final long bestRising = nvl( risingEdgeStats.getHighestRanked(), 0L );

    final Frequency<Long> fallingEdgeStats = this.stats.get( Edge.FALLING );
    final long bestFalling = nvl( fallingEdgeStats.getHighestRanked(), 0L );

    System.out.println( "" );

    return new ClockStats( bestRising, bestFalling, getSampleRate() );
  }
}
