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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.nrza;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.tool.api.*;


/**
 * Represents a NRZA-decoder.
 */
public class NonReturnToZeroDecoderTask implements ToolTask<AcquisitionData>
{
  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;
  private final ToolAnnotationHelper annHelper;

  private int dataIdx;
  private int clockIdx;
  private boolean inverted;

  // CONSTRUCTORS

  /**
   * Creates a new NonReturnToZeroDecoderTask instance.
   */
  public NonReturnToZeroDecoderTask( final ToolContext aContext, final ToolProgressListener aProgressListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
    this.annHelper = new ToolAnnotationHelper( aContext );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData call() throws Exception
  {
    final AcquisitionData inputData = this.context.getData();

    final int[] values = inputData.getValues();
    final long[] timestamps = inputData.getTimestamps();

    final int dataMask = ( 1 << this.dataIdx );
    final int clockMask = ( 1 << this.clockIdx );

    this.annHelper.clearAnnotations( this.dataIdx, this.clockIdx );

    int startIdx = this.context.getStartSampleIndex();
    int endIdx = this.context.getEndSampleIndex();

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
          this.annHelper.addAnnotation( this.dataIdx, timestamps[symbolStartIdx], timestamps[i],
              String.format( "%1$c (%1$x)", Integer.valueOf( symbol ) ) );

          symbol = 0;
          bitCount = 0;
          symbolStartIdx = -1;
        }
      }
    }

    return null;
  }

  public void setClockIdx( int aClockIdx )
  {
    this.clockIdx = aClockIdx;
  }

  public void setDataIdx( int aDataIdx )
  {
    this.dataIdx = aDataIdx;
  }

  public void setInverted( boolean aInverted )
  {
    this.inverted = aInverted;
  }
}
