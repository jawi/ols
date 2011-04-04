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

  /**
   * 
   */
  static class ClockPeriodStats implements Comparable<ClockPeriodStats>
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
      result = prime * result + ( int )( this.period1 ^ ( this.period1 >>> 32 ) );
      result = prime * result + ( int )( this.period2 ^ ( this.period2 >>> 32 ) );
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

  // VARIABLES

  private final int channelMask;
  private final Frequency<ClockPeriodStats> periodStats;

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

    this.periodStats = new Frequency<ClockPeriodStats>();
  }

  // METHODS

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
    int lastBitValue = values[i++] & this.channelMask;

    Long prevPeriodTime = null;

    for ( ; i < end; i++ )
    {
      final int bitValue = values[i] & this.channelMask;

      if ( lastBitValue != bitValue )
      {
        final long periodTime = timestamps[i] - lastTransition;

        if ( prevPeriodTime != null )
        {
          final ClockPeriodStats stats = new ClockPeriodStats( prevPeriodTime.longValue(), periodTime );
          this.periodStats.addValue( stats );

          prevPeriodTime = null;
        }
        else
        {
          prevPeriodTime = Long.valueOf( periodTime );
        }

        lastTransition = timestamps[i];
      }

      lastBitValue = bitValue;
    }

    final ClockPeriodStats best = this.periodStats.getHighestRanked();

    System.out.println( "" + best );

    return new ClockStats( best.getPeriod1(), best.getPeriod2(), getSampleRate() );
  }
}
