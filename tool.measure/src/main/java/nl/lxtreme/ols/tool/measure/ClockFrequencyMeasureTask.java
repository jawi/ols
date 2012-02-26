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


import static nl.lxtreme.ols.util.DisplayUtils.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.api.util.*;


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
    // VARIABLES

    private final double dutycycle;
    private final double frequency;
    private final double error;
    private final double measuringTime;
    private final int pulseCount;
    private final int risingEdgeCount;
    private final int fallingEdgeCount;

    /**
     * Creates a new ClockStats instance.
     * 
     * @param aPeriodStats
     *          the (best) period statistics;
     * @param aSampleRate
     *          the sample rate to use.
     */
    private ClockStats( final double aMeasuringTime, final double aFrequency, final double aDutyCycle,
        final int aPulseCount, final int aRisingEdgeCount, final int aFallingEdgeCount )
    {
      this.measuringTime = aMeasuringTime;
      this.frequency = aFrequency;
      this.dutycycle = aDutyCycle;
      this.pulseCount = aPulseCount;
      this.risingEdgeCount = aRisingEdgeCount;
      this.fallingEdgeCount = aFallingEdgeCount;
      this.error = Math.abs( ( this.pulseCount / this.measuringTime ) - this.frequency ) / 2.0;
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
      return this.dutycycle;
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
      return this.error;
    }

    /**
     * Returns the current value of fallingEdgeCount.
     * 
     * @return the falling edge count, never <code>null</code>.
     */
    public Integer getFallingEdgeCount()
    {
      return Integer.valueOf( this.fallingEdgeCount );
    }

    /**
     * Returns the measured frequency.
     * 
     * @return the measured frequency, in Hertz.
     */
    public double getFrequency()
    {
      return this.frequency;
    }

    /**
     * Returns the current value of measuringTime.
     * 
     * @return the measuringTime
     */
    public double getMeasuringTime()
    {
      return this.measuringTime;
    }

    /**
     * Returns the current value of pulseCount.
     * 
     * @return the pulse count, never <code>null</code>.
     */
    public Integer getPulseCount()
    {
      return Integer.valueOf( this.pulseCount );
    }

    /**
     * Returns the current value of risingEdgeCount.
     * 
     * @return the rising edge count, never <code>null</code>.
     */
    public Integer getRisingEdgeCount()
    {
      return Integer.valueOf( this.risingEdgeCount );
    }

    /**
     * Returns whether or not there's an error in the frequency measurement.
     * 
     * @return <code>true</code> if there is an error, <code>false</code>
     *         otherwise.
     */
    public boolean hasError()
    {
      return Math.abs( this.error ) >= 0.01;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString()
    {
      String timeText = UnitOfTime.toString( this.measuringTime );
      String frequencyText = FrequencyUnit.toString( this.frequency );
      if ( hasError() )
      {
        frequencyText = frequencyText.concat( " (error = \u00b1" ).concat( FrequencyUnit.toString( this.error ) )
            .concat( ")" );
      }
      String dutyCycleText = displayPercentage( this.dutycycle );

      return String.format( "Measure time: %s; frequency: %s; dutycycle: %s; # of pulses: %d (\u2191%d, \u2193%d)",
          timeText, frequencyText, dutyCycleText, Integer.valueOf( this.pulseCount ),
          Integer.valueOf( this.risingEdgeCount ), Integer.valueOf( this.fallingEdgeCount ) );
    }
  }

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ClockFrequencyMeasureTask.class.getName() );

  // VARIABLES

  private final ToolContext context;

  private int channelMask;

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
  }

  // METHODS

  /**
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public ClockStats call() throws Exception
  {
    final Cursor cursorA = this.context.getCursor( 0 );
    final Cursor cursorB = this.context.getCursor( 1 );

    final AcquisitionResult data = this.context.getData();

    final int[] values = data.getValues();
    final long[] timestamps = data.getTimestamps();

    final long startTimestamp;
    final long endTimestamp;

    final int start;
    final int end;

    if ( ( cursorA.isDefined() ) && ( cursorB.isDefined() ) )
    {
      startTimestamp = cursorA.getTimestamp();
      start = data.getSampleIndex( startTimestamp );

      endTimestamp = cursorB.getTimestamp();
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

    int highCount = 0;
    long highTime = 0;
    int lowCount = 0;
    long lowTime = 0;

    int i = start;
    long lastTransition = timestamps[i];
    int lastBitValue = values[i++] & this.channelMask;

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
    double avgHighTime = ( highTime / ( double )highCount );
    double avgLowTime = ( lowTime / ( double )lowCount );

    double frequency = data.getSampleRate() / ( avgHighTime + avgLowTime );
    double dutyCycle = avgHighTime / ( avgHighTime + avgLowTime );

    final ClockStats clockStats = new ClockStats( measureTime, frequency, dutyCycle, pulseCount, lowCount, highCount );

    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( clockStats.toString() );
    }

    return clockStats;
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
