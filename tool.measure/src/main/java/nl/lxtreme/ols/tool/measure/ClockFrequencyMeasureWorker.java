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
    ClockStats( final long aRising, final long aFalling, final long aSampleRate )
    {
      this.rising = aRising;
      this.falling = aFalling;

      this.period = ( this.rising + this.falling );
      this.frequency = aSampleRate / ( double )( this.rising + this.falling );
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

    // Make sure no other stuff from previous runs keep lingering...
    this.stats.get( Edge.RISING ).clear();
    this.stats.get( Edge.FALLING ).clear();

    int i = Math.max( 0, start - 1 );
    long lastTransition = 0;
    int lastBitValue = values[i] & this.channelMask;

    for ( ; i < end; i++ )
    {
      final int bitValue = values[i] & this.channelMask;

      if ( lastBitValue != bitValue )
      {
        final Edge edge;
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

        final Frequency<Long> edgeStats = this.stats.get( edge );
        edgeStats.addValue( Long.valueOf( ( timestamps[i] - lastTransition ) ) );

        lastTransition = timestamps[i];
      }

      lastBitValue = bitValue;
    }

    final Long bestRising = this.stats.get( Edge.RISING ).getHighestRanked();
    final Long bestFalling = this.stats.get( Edge.FALLING ).getHighestRanked();

    return new ClockStats( ( bestRising == null ) ? 0 : bestRising.longValue(), //
        ( bestFalling == null ) ? 0 : bestFalling.longValue(), getSampleRate() );
  }
}
