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


import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.linedecoder.*;
import nl.lxtreme.ols.util.analysis.*;


/**
 * Represents a Manchester line decoder.
 */
public class ManchesterLineDecoder implements LineDecoder
{
  // INNER TYPES

  static enum State
  {
    IDLE, SYNC, DECODE, ERROR;
  }

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
  public AcquisitionResult decode( final LineDecoderToolContext aContext, final ToolProgressListener aListener )
      throws Exception
  {
    final int lineIdx = aContext.getLineChannels()[0];
    final boolean inverted = aContext.isInverted();

    final int mask = ( 1 << lineIdx );

    final int startIdx = aContext.getStartSampleIndex();
    final int endIdx = aContext.getEndSampleIndex();

    final AcquisitionResult inputData = aContext.getData();

    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();

    Frequency<Long> frequencyMap = new Frequency<Long>();

    int oldIndex = startIdx;
    int oldValue = values[startIdx] & mask;

    // Pass 1: count the various low/high times...
    for ( int i = startIdx + 1; i < endIdx; i++ )
    {
      int value = values[i] & mask;
      if ( value != oldValue )
      {
        long t1 = timestamps[oldIndex];
        long t2 = timestamps[i];

        frequencyMap.addValue( Long.valueOf( t2 - t1 ) );

        oldIndex = i;
        oldValue = value;
      }
    }

    // Pass 2: determine the smallest and highest times...
    long min = Long.MAX_VALUE;
    long max = Long.MIN_VALUE;
    long threshold = frequencyMap.getTotalCount() / 10;
    for ( Long value : frequencyMap.values() )
    {
      if ( frequencyMap.getCount( value ) < threshold )
      {
        // Skip spurious values...
        continue;
      }
      final long v = value.longValue();
      min = Math.min( min, v );
      max = Math.max( max, v );
    }

    // Pass 3: decode the data using the minimal and maximal timing values...
    oldIndex = startIdx;
    oldValue = ( values[startIdx] & mask ) >> lineIdx;
    boolean inSymbol = false;
    int curSymbol = 0;
    int curBitCount = 0;

    for ( int i = startIdx + 1; i < endIdx; i++ )
    {
      int curValue = ( values[i] & mask ) >> lineIdx;

      Edge e = Edge.toEdge( oldValue, curValue );
      if ( !e.isNone() )
      {
        long t = timestamps[i] - timestamps[oldIndex];

        if ( inWindow( min, t ) )
        {
          if ( !inSymbol )
          {
            // sample point;
            inSymbol = true;

            curSymbol <<= 1;
            curSymbol |= ( inverted ? oldValue : curValue );
            curBitCount++;
          }
          else
          {
            // bit boundary;
            inSymbol = false;
          }
        }
        else if ( inWindow( max, t ) )
        {
          // If we're in the middle of a symbol; we stay in the middle of a
          // symbol...
          if ( inSymbol )
          {
            curSymbol <<= 1;
            curSymbol |= ( inverted ? oldValue : curValue );
            curBitCount++;
          }
        }
        else
        {
          inSymbol = false;
        }

        if ( curBitCount == 9 )
        {
          curSymbol &= 0xFF;
          System.out.println( "DECODED = " + ( char )curSymbol + " (0x" + Integer.toHexString( curSymbol ) + ")" );
          curSymbol = 0;
          curBitCount = 0;
        }

        oldIndex = i;
        oldValue = curValue;
      }
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

  private boolean inWindow( final long aMatchedValue, final long aValue )
  {
    return Math.abs( aMatchedValue - aValue ) < 5L;
  }
}
