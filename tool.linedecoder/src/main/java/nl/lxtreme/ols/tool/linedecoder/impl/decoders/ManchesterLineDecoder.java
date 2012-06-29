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
package nl.lxtreme.ols.tool.linedecoder.impl.decoders;


import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.annotation.*;
import nl.lxtreme.ols.tool.linedecoder.*;


/**
 * Represents a Manchester line decoder.
 */
public class ManchesterLineDecoder implements LineDecoder
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canHandleInversion()
  {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canRecoverClock()
  {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult decode( final LineDecoderToolContext aContext, final AnnotationListener aAnnotationListener,
      final ToolProgressListener aListener ) throws Exception
  {
    final AcquisitionResult inputData = aContext.getData();

    final int expectedBitSize = ( int )( ( 1.0 / aContext.getClockSpeed() ) * inputData.getSampleRate() );

    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();

    int startIdx = aContext.getStartSampleIndex();
    int endIdx = aContext.getEndSampleIndex();
    // Find first edge...
    int oldValue = values[startIdx];
    while ( ( startIdx < endIdx ) && ( values[startIdx] == oldValue ) )
    {
      startIdx++;
    }

    final int channelIdx = aContext.getLineChannels()[0];

    aAnnotationListener.clearAnnotations( channelIdx );

    int symbolSize = 8;
    int bitCount = -1;
    int symbol = 0;

    long startTime = timestamps[startIdx];
    long endTime = timestamps[endIdx];
    long timePtr = startTime + expectedBitSize;

    while ( timePtr < endTime )
    {
      int value = getDataValue( aContext, timePtr );
      if ( ++bitCount == symbolSize )
      {
        aAnnotationListener.onAnnotation( new SampleDataAnnotation( channelIdx, startTime, timePtr - expectedBitSize,
            String.format( "%1$c (%1$x)", Integer.valueOf( symbol ) ) ) );

        symbol = 0;
        bitCount = 0;
        startTime = timePtr - expectedBitSize;
      }
      symbol <<= 1;
      if ( value != 0 )
      {
        symbol |= 1;
      }
      timePtr += expectedBitSize;
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLineCount()
  {
    return 1;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Manchester";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean needClockSpeed()
  {
    return true;
  }

  /**
   * Returns the data value for the given time stamp.
   * 
   * @param aTimeValue
   *          the time stamp to return the data value for.
   * @return the data value of the sample index right before the given time
   *         value.
   */
  protected final int getDataValue( final LineDecoderToolContext aContext, final long aTimeValue )
  {
    final AcquisitionResult inputData = aContext.getData();
    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();
    final int mask = ( 1 << aContext.getLineChannels()[0] );

    int k = Arrays.binarySearch( timestamps, aTimeValue );
    if ( k < 0 )
    {
      k = -( k + 1 );
    }

    int value = ( ( k == 0 ) ? values[0] : values[k - 1] );
    if ( aContext.isInverted() )
    {
      value = ~value;
    }

    return value & mask;
  }
}
