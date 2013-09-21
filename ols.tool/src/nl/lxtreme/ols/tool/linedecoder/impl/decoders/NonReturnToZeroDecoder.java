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
import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.tool.base.annotation.*;
import nl.lxtreme.ols.tool.linedecoder.*;


/**
 * Provides a non-return to zero decoder, which is simply a data- and a
 * clock-line at directly encode the data.
 */
public class NonReturnToZeroDecoder implements LineDecoder
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
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult decode( final LineDecoderToolContext aContext, final AnnotationListener aAnnotationListener,
      final ToolProgressListener aListener ) throws Exception
  {
    final AcquisitionResult inputData = aContext.getData();

    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();

    final int dataIdx = aContext.getLineChannels()[0];
    final int clockIdx = aContext.getLineChannels()[1];

    final int dataMask = ( 1 << dataIdx );
    final int clockMask = ( 1 << clockIdx );

    aAnnotationListener.clearAnnotations( dataIdx );

    int startIdx = aContext.getStartSampleIndex();
    int endIdx = aContext.getEndSampleIndex();

    int symbolStartIdx = -1;
    int symbolSize = 8;
    int bitCount = 0;
    int symbol = 0;

    for ( int i = startIdx; i < endIdx; i++ )
    {
      int clockValue = values[i] & clockMask;

      if ( clockValue != 0 )
      {
        if ( symbolStartIdx < 0 )
        {
          symbolStartIdx = i;
        }

        int dataValue = values[i] & dataMask;

        symbol <<= 1;
        if ( dataValue != 0 )
        {
          symbol |= 1;
        }
        bitCount++;

        if ( bitCount == symbolSize )
        {
          aAnnotationListener.onAnnotation( createAnnotation( dataIdx, timestamps[symbolStartIdx], timestamps[i],
              symbol ) );

          symbol = 0;
          bitCount = 0;
          symbolStartIdx = -1;
        }
      }
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String[] getLineNames()
  {
    return new String[] { "Data", "Clock" };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Non-return to zero";
  }

  /**
   * @param aIndex
   * @param aStartTime
   * @param aEndTime
   * @param aSymbol
   * @return
   */
  private SampleDataAnnotation createAnnotation( final int aIndex, final long aStartTime, final long aEndTime,
      final int aSymbol )
  {
    return new SampleDataAnnotation( aIndex, aStartTime, aEndTime, String.format( "%1$c (%1$x)",
        Integer.valueOf( aSymbol ) ) );
  }
}
