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
package nl.lxtreme.ols.tool.measure;


import java.util.logging.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.analysis.*;


/**
 * Provides a background worker for measuring the clock frequency of a signal
 * channel.
 */
public class ClockFrequencyMeasureTask implements ToolTask<ClockFrequencyMeasureTask.ClockStats>
{
  // INNER TYPES

  /**
   * Provides some statistics on a measured clock frequency.
   */
  public static class ClockStats
  {
    // CONSTANTS

    /** Denotes a half sampling period (unitless). */
    private static final double HALF_PERIOD = 0.5;

    // VARIABLES

    private final ClockPeriodStats periodStats;
    private final double sampleRate;

    /**
     * Creates a new ClockStats instance.
     * 
     * @param aPeriodStats
     *          the (best) period statistics;
     * @param aSampleRate
     *          the sample rate to use.
     */
    private ClockStats( final ClockPeriodStats aPeriodStats, final long aSampleRate )
    {
      this.periodStats = aPeriodStats;
      this.sampleRate = aSampleRate;
    }

    // METHODS

    /**
     * Returns the duty cycle of the measured clock signal, meaning the
     * percentage of "active" part of the pulse with respect to the total pulse
     * width.
     * 
     * @return a duty cycle, as percentage between 0.0 and 1.0.
     */
    public double getDutyCycle()
    {
      if ( this.periodStats == null )
      {
        return 0.0;
      }

      final double totalPeriod = this.periodStats.getTotalPeriod();
      if ( totalPeriod == 0.0 )
      {
        return 0.0;
      }

      final double period = Math.max( this.periodStats.getPeriod1(), this.periodStats.getPeriod2() );
      return period / totalPeriod;
    }

    /**
     * Returns the error of the measured frequency meaning that the
     * <em>actual</em> frequency is somewhere between the measured frequency
     * plus or minus this error.
     * <p>
     * The error is determined by
     * <tt>(F<sub>upper</sub> - F<sub>lower</sub>) / 2.0</tt>, where
     * <tt>F<sub>upper</sub></tt> is the upper frequency bound, and
     * <tt>F<sub>lower</sub></tt> is the lower frequency bound.
     * </p>
     * 
     * @return the measured error, in Hertz.
     * @see #getLowerBoundFrequency()
     * @see #getUpperBoundFrequency()
     */
    public double getError()
    {
      if ( this.periodStats == null )
      {
        return 0.0;
      }

      final double tau = this.periodStats.getTotalPeriod();
      // Simplified version of expression denoted in JavaDoc...
      return this.sampleRate / ( ( 2 * tau * tau ) - 0.5 );
    }

    /**
     * Returns the measured frequency.
     * 
     * @return the measured frequency, in Hertz.
     */
    public double getFrequency()
    {
      if ( this.periodStats == null )
      {
        return 0.0;
      }

      final long totalPeriod = this.periodStats.getTotalPeriod();
      if ( totalPeriod == 0 )
      {
        return 0.0;
      }

      return this.sampleRate / totalPeriod;
    }

    /**
     * Returns the "lower bound" of the measured frequency.
     * <p>
     * The measured frequency has a certain uncertainty, due to the discrete
     * sampling of the original signal. This uncertainty can be represented by a
     * lower and upper bound.
     * </p>
     * <p>
     * The lower bound is calculated by making the measured signal period
     * "slightly" larger, and divide this by the sample frequency. The upper
     * bound is calculated similarly, making the measured period "slightly"
     * shorter. Note that these calculated frequencies are optimistic values, as
     * the period is enlarged/shortened by <em>half</em> the sampling period.
     * </p>
     * 
     * @return a lower bound frequency, in Hertz.
     */
    public double getLowerBoundFrequency()
    {
      if ( this.periodStats == null )
      {
        return 0.0;
      }

      final double period = this.periodStats.getTotalPeriod() + HALF_PERIOD;
      return this.sampleRate / period;
    }

    /**
     * Returns the "upper bound" of the measured frequency.
     * 
     * @return a upper bound frequency, in Hertz.
     * @see #getLowerBoundFrequency()
     */
    public double getUpperBoundFrequency()
    {
      if ( this.periodStats == null )
      {
        return 0.0;
      }

      final double period = this.periodStats.getTotalPeriod() - HALF_PERIOD;
      return this.sampleRate / period;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      StringBuilder sb = new StringBuilder();
      sb.append( "Clock statistics: " ).append( DisplayUtils.displayFrequency( getFrequency() ) );
      sb.append( " (" ).append( DisplayUtils.displayPercentage( getDutyCycle() ) ).append( "), " );
      sb.append( "error = \u00b1" ).append( DisplayUtils.displayFrequency( getError() ) );
      return sb.toString();
    }
  }

  /**
   * Container for the period statistics.
   */
  private static class ClockPeriodStats implements Comparable<ClockPeriodStats>
  {
    // VARIABLES

    private final long period1;
    private final long period2;

    // CONSTRUCTORS

    /**
     * Creates a new ClockFrequencyMeasureWorker.ClockPeriodStats instance.
     */
    public ClockPeriodStats( final long aPeriod1, final long aPeriod2 )
    {
      this.period1 = aPeriod1;
      this.period2 = aPeriod2;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final ClockPeriodStats aOtherStats )
    {
      long p1 = getTotalPeriod();
      long p2 = aOtherStats.getTotalPeriod();
      int result = ( int )( p1 - p2 );
      if ( result == 0 )
      {
        p1 = getPeriod1();
        p2 = aOtherStats.getPeriod1();
        result = ( int )( p1 - p2 );
        if ( result == 0 )
        {
          p1 = getPeriod2();
          p2 = aOtherStats.getPeriod2();
          result = ( int )( p1 - p2 );
        }
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( final Object aObject )
    {
      if ( this == aObject )
      {
        return true;
      }
      if ( ( aObject == null ) || !( aObject instanceof ClockPeriodStats ) )
      {
        return false;
      }

      final ClockPeriodStats other = ( ClockPeriodStats )aObject;
      if ( this.period1 != other.period1 )
      {
        return false;
      }
      if ( this.period2 != other.period2 )
      {
        return false;
      }
      return true;
    }

    /**
     * Returns the current value of period1.
     * 
     * @return the period1
     */
    public long getPeriod1()
    {
      return this.period1;
    }

    /**
     * Returns the current value of period2.
     * 
     * @return the period2
     */
    public long getPeriod2()
    {
      return this.period2;
    }

    /**
     * Returns the total period value.
     * 
     * @return the value of period1 + period2.
     */
    public long getTotalPeriod()
    {
      return this.period1 + this.period2;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + ( int )( this.period1 ^ ( this.period1 >>> 32 ) );
      result = ( prime * result ) + ( int )( this.period2 ^ ( this.period2 >>> 32 ) );
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      return String.format( "[%d, %d] => (%.3f)", Long.valueOf( this.period1 ), Long.valueOf( this.period2 ),
          Double.valueOf( this.period1 / ( double )( this.period2 + this.period1 ) ) );
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ClockFrequencyMeasureTask.class.getName() );

  // VARIABLES

  private final ToolContext context;

  private int channelMask;
  private final Frequency<ClockPeriodStats> periodStats;

  // CONSTRUCTORS

  /**
   * Creates a new ClockFrequencyMeasureTask instance.
   * 
   * @param aContext
   *          the tool context to use.
   */
  public ClockFrequencyMeasureTask( final ToolContext aContext )
  {
    this.context = aContext;

    this.periodStats = new Frequency<ClockPeriodStats>();
  }

  // METHODS

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public ClockStats call() throws Exception
  {
    final Long cursorA = this.context.getCursorPosition( 0 );
    final Long cursorB = this.context.getCursorPosition( 1 );

    final AcquisitionResult data = this.context.getData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    final long startTimestamp;
    final long endTimestamp;

    final int start;
    final int end;

    if ( ( cursorA != null ) && ( cursorB != null ) )
    {
      startTimestamp = cursorA.longValue();
      start = data.getSampleIndex( startTimestamp );

      endTimestamp = cursorB.longValue();
      end = data.getSampleIndex( endTimestamp );
    }
    else
    {
      start = this.context.getStartSampleIndex();
      startTimestamp = timestamps[start];

      end = this.context.getEndSampleIndex();
      endTimestamp = timestamps[end];
    }

    final double measureTime = Math.abs( endTimestamp - startTimestamp ) / ( double )data.getSampleRate();

    int i = start;
    int lastBitValue = values[i++] & this.channelMask;

    long lastTransition = 0;

    int highCount = 0;
    long highTime = 0;
    int lowCount = 0;
    long lowTime = 0;

    for ( ; !Thread.currentThread().isInterrupted() && ( i <= end ); i++ )
    {
      final int bitValue = values[i] & this.channelMask;

      if ( lastBitValue != bitValue )
      {
        final long periodTime = timestamps[i] - lastTransition;

        if ( lastBitValue < bitValue )
        {
          // Low to high transition: previously seen a low-state...
          lowCount++;
          lowTime += periodTime;
        }
        else if ( lastBitValue > bitValue )
        {
          // High to low transition: previously seen a high-state...
          highCount++;
          highTime += periodTime;
        }

        lastTransition = timestamps[i];
      }

      lastBitValue = bitValue;
    }

    int pulseCount = ( lowCount + highCount ) / 2;

    // Take the average high & low time per pulse...
    double r = ( highTime / ( double )highCount );
    double s = ( lowTime / ( double )lowCount );

    double f = data.getSampleRate() / ( r + s );

    double e = Math.abs( ( pulseCount / measureTime ) - f );

    if ( LOG.isLoggable( Level.INFO ) )
    {
      String timeText = DisplayUtils.displayTime( measureTime );
      String frequencyText = DisplayUtils.displayFrequency( f );
      String dutyCycleText = String.format( "%.3f%%", Double.valueOf( ( 100.0 * r ) / ( r + s ) ) );
      String error = DisplayUtils.displayFrequency( e );
      String pulseCountText = Integer.toString( pulseCount );

      LOG.info( String.format( "Measure time: %s; # of pulses: %s; frequency: %s (error = %s); dutycycle: %s",
          timeText, pulseCountText, frequencyText, error, dutyCycleText ) );
    }

    final ClockPeriodStats best = this.periodStats.getHighestRanked();
    return new ClockStats( best, data.getSampleRate() );
  }

  /**
   * Sets the channel to measure the clock frequency for.
   * 
   * @param aChannelIdx
   *          a channel index, >= 0 && < 32.
   */
  public void setChannel( final int aChannelIdx )
  {
    this.channelMask = ( 1 << aChannelIdx );
  }
}
