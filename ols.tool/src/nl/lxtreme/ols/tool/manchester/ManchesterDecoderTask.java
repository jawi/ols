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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.manchester;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.util.*;
import nl.lxtreme.ols.tool.api.*;
import aQute.bnd.annotation.metatype.*;


/**
 * Represents a Manchester line decoder.
 */
public class ManchesterDecoderTask implements Callable<Void>
{
  // VARIABLES

  private final ToolContext context;

  private final boolean recoverClock;
  private final int clockSpeed;
  private final int dataIdx;
  private final int clockIdx;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ManchesterDecoderTask} instance.
   */
  public ManchesterDecoderTask( final ToolContext aContext, final Configuration aConfiguration )
  {
    this.context = aContext;

    ManchesterConfig config = Configurable.createConfigurable( ManchesterConfig.class, aConfiguration.asMap() );

    this.recoverClock = config.recoverClock();
    this.clockSpeed = config.clock();
    this.dataIdx = config.dataIdx();
    this.clockIdx = config.clockIdx();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @SuppressWarnings( "boxing" )
  @Override
  public Void call() throws ToolException
  {
    final AcquisitionData inputData = this.context.getAcquisitionData();
    final ToolAnnotationHelper annotationHelper = new ToolAnnotationHelper( this.context );

    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();

    final int dataMask = ( 1 << this.dataIdx );
    final int clockMask = ( 1 << this.clockIdx );

    this.context.clearAnnotations( this.dataIdx, this.clockIdx );

    int startIdx = this.context.getStartSampleIndex();
    int endIdx = this.context.getEndSampleIndex();
    int lastValue = values[startIdx] & dataMask;

    long symbolStartTime = -1L;
    long lastTimestamp = -1L;
    long firstSignalEdge = -1L;
    long halfCycle = -1L;
    long jitter = 0L;
    int edgeCounter = 0;

    int symbolSize = 8;
    int bitCount = 0;
    int symbol = 0;

    for ( int i = startIdx; i < endIdx; i++ )
    {
      int value = values[i] & dataMask;

      long clockEdge = -1L;

      final Edge edge = Edge.toEdge( lastValue, value );
      if ( !edge.isNone() )
      {
        if ( lastTimestamp < 0L )
        {
          // First rising or falling edge; take its timestamp and do not do
          // anything yet, we need another edge to fully start the decoding
          // process...
          lastTimestamp = timestamps[i];
          symbolStartTime = lastTimestamp;
          firstSignalEdge = lastTimestamp;
        }
        else
        {
          // Either a falling or rising edge; take the time between the former
          // edge and this edge.
          long diff = timestamps[i] - lastTimestamp;

          if ( halfCycle <= 0L )
          {
            // Initialization: we've not calculated a half-cycle before (or let
            // it diverge to zero), so lets presume the current difference is an
            // indication for it. We divide the timestamp by two to ensure we
            // always start with T...
            halfCycle = edge.isFalling() ? diff / 2 : diff;
            // Assume an initial value for our jitter coefficient...
            jitter = diff / 2;
          }
          else
          {
            // The difference should either be T (+/- jitter) or 2*T (+/-
            // jitter)...
            if ( ( diff >= ( halfCycle - jitter ) ) && ( diff <= ( halfCycle + jitter ) ) )
            {
              halfCycle = diff;
              jitter = diff / 8;

              // Only the even edges are considered a clock edge...
              if ( ( edgeCounter % 2 ) == 0 )
              {
                clockEdge = timestamps[i];
              }
              edgeCounter++;
            }
            else if ( ( diff >= ( 2 * ( halfCycle - jitter ) ) ) && ( diff <= ( 2 * ( halfCycle + jitter ) ) ) )
            {
              halfCycle = diff / 2;
              jitter = diff / 16;

              // The clock edge should have appeared halfCycle before the
              // current timestamp...
              clockEdge = timestamps[i] - halfCycle;
              // We've missed a clock edge, so increase the counter by 2...
              edgeCounter += 2;
            }
          }

          lastTimestamp = timestamps[i];
        }
      }

      if ( clockEdge >= 0L )
      {
        int sampleValue = getDataValue( this.context, clockEdge );

        symbol <<= 1;
        bitCount++;
        if ( ( sampleValue & dataMask ) != 0 )
        {
          symbol |= 1;
        }

        if ( bitCount == symbolSize )
        {
          annotationHelper.addSymbolAnnotation( this.dataIdx, symbolStartTime, clockEdge, symbol );

          symbol = 0;
          bitCount = 0;
          symbolStartTime = clockEdge;
        }
      }

      lastValue = value;
    }

    lastTimestamp += halfCycle;

    String format = Unit.Frequency.format( inputData.getSampleRate() / ( 2.0 * halfCycle ) );
    System.out.println( "Clock signal = " + format );

    SortedMap<Long, Integer> newSamples = new TreeMap<Long, Integer>();
    for ( int i = 0; i < values.length; i++ )
    {
      newSamples.put( timestamps[i], values[i] );
    }

    boolean clockLow = true;
    for ( long time = firstSignalEdge; time < lastTimestamp; time += halfCycle )
    {
      int sampleValue = getDataValue( this.context, time );
      if ( clockLow )
      {
        sampleValue &= ~clockMask;
      }
      else
      {
        sampleValue |= clockMask;
      }
      clockLow = !clockLow;

      newSamples.put( time, sampleValue );
    }

    // 2nd pass: XOR data with clock...
    for ( Long time : newSamples.keySet() )
    {
      int sampleValue = newSamples.get( time );

      int clockValue = sampleValue & clockMask;
      int dataValue = sampleValue & dataMask;

      if ( ( ( clockValue != 0 ) && ( dataValue == 0 ) ) || ( ( clockValue == 0 ) && ( dataValue != 0 ) ) )
      {
        sampleValue |= 1;
      }
      else
      {
        sampleValue &= 0xFE;
      }

      newSamples.put( time, sampleValue );
    }

    AcquisitionDataBuilder builder = new AcquisitionDataBuilder( inputData, false /* aIncludeSamples */);

    for ( Map.Entry<Long, Integer> entry : newSamples.entrySet() )
    {
      builder.addSample( entry.getKey(), entry.getValue() );
    }

    for ( int i = endIdx; i < values.length; i++ )
    {
      builder.addSample( timestamps[i], values[i] );
    }

    // Update the acquisition data...
    this.context.getSession().setAcquisitionData( builder.build() );

    return null;
  }

  /**
   * Returns the data value for the given time stamp.
   * 
   * @param aTimeValue
   *          the time stamp to return the data value for.
   * @return the data value of the sample index right before the given time
   *         value.
   */
  protected final int getDataValue( final ToolContext aContext, final long aTimeValue )
  {
    final AcquisitionData inputData = aContext.getAcquisitionData();
    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();

    int k = Arrays.binarySearch( timestamps, aTimeValue );
    if ( k < 0 )
    {
      k = -( k + 1 );
    }

    return ( ( k == 0 ) ? values[0] : values[k - 1] );
  }
}
