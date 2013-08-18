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
package org.sump.device.logicsniffer.sampleprocessor;


import java.util.logging.*;

import org.sump.device.logicsniffer.*;


/**
 * Provides a RLE decoder.
 */
public final class RleDecoder implements SampleProcessor
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( RleDecoder.class.getName() );

  // VARIABLES

  private final LogicSnifferConfig config;
  private final int[] buffer;
  private final int trigCount;
  private final SampleProcessorCallback callback;

  private final int rleCountValue;
  private final int rleCountMask;

  // CONSTRUCTORS

  /**
   * Creates a new RleDecoder instance.
   * 
   * @param aConfig
   * @param aBuffer
   * @param aTrigCount
   * @param aCallback
   */
  public RleDecoder( final LogicSnifferConfig aConfig, final int[] aBuffer, final int aTrigCount,
      final SampleProcessorCallback aCallback )
  {
    if ( aBuffer == null )
    {
      throw new IllegalArgumentException( "Buffer cannot be null!" );
    }

    this.config = aConfig;
    this.buffer = aBuffer;
    this.trigCount = aTrigCount;
    this.callback = aCallback;

    // enabled group count is "automatically" corrected for DDR/Demux mode...
    final int width = this.config.getRLEDataWidth();
    switch ( width )
    {
      case 32:
        this.rleCountValue = 0x80000000;
        this.rleCountMask = this.rleCountValue - 1;
        break;
      case 24:
        this.rleCountValue = 0x800000;
        this.rleCountMask = this.rleCountValue - 1;
        break;
      case 16:
        this.rleCountValue = 0x8000;
        this.rleCountMask = this.rleCountValue - 1;
        break;
      case 8:
        this.rleCountValue = 0x80;
        this.rleCountMask = this.rleCountValue - 1;
        break;
      default:
        throw new IllegalArgumentException( "Illegal RLE width! Should be 8, 16, 24 or 32!" );
    }
  }

  // METHODS

  /**
   * @see org.sump.device.logicsniffer.sampleprocessor.SampleProcessor#process()
   */
  public void process()
  {
    long time = 0;
    long rleTrigPos = 0;
    int lastSample = -1;

    // if msb set increment time by the count value
    // else save sample check trigger pos and increment time by 1
    // this should work for either dogsbody or rasmus bitstreams

    final int samples = this.buffer.length;

    // shiftBits needs to be 8 if 8 bit selected and 16 if 16 bit selected
    final int rleShiftBits = this.config.getRLEDataWidth();
    final boolean ddrMode = this.config.isDoubleDataRateEnabled();

    for ( int i = 0; i < samples; i++ )
    {
      final int sampleValue = this.buffer[i];
      final int normalizedSampleValue = normalizeSampleValue( sampleValue );

      // if a count just add it to the time
      if ( ( normalizedSampleValue & this.rleCountValue ) != 0 )
      {
        long count = ( normalizedSampleValue & this.rleCountMask );
        if ( ddrMode && ( i < ( samples - 1 ) ) )
        {
          // In case of "double data rate", the RLE-counts are encoded as 16-
          // resp. 32-bit values, so we need to take two samples for each
          // count (as they are 8- or 16-bits in DDR mode).
          // This should also solve issue #31...

          // Issue #55: double the RLE-count as we're using DDR mode which
          // takes two samples in one time period...
          long ddrCount = ( ( count << rleShiftBits ) | normalizeSampleValue( this.buffer[++i] ) );
          count = 2L * ddrCount;
        }

        if ( lastSample >= 0 )
        {
          time += count;
        }
        else
        {
          LOG.warning( "Ignoring RLE count without preceeding sample value: " + Long.toHexString( count ) );
        }
      }
      else
      {
        // this is a data value only save data if different to last
        if ( sampleValue != lastSample )
        {
          // set the trigger position as a time value
          if ( ( i >= this.trigCount ) && ( rleTrigPos == 0 ) )
          {
            rleTrigPos = time;
          }

          // add the read sample & add a timestamp value as well...
          this.callback.addValue( sampleValue, time );
          lastSample = sampleValue;
        }
        time++;
      }
    }

    // Ensure the last sample is shown as well (even if there was a lot of time
    // between the last real sample and the end of the capture; i.e., constant
    // data)...
    this.callback.addValue( lastSample, time );

    // Take the last seen time value as "absolete" length of this trace...
    this.callback.ready( time, rleTrigPos - 1 );
  }

  /**
   * Normalizes the given sample value to mask out the unused channel groups and
   * get a sample value in the correct width.
   * 
   * @param aSampleValue
   *          the original sample to normalize.
   * @return the normalized sample value.
   */
  private int normalizeSampleValue( final int aSampleValue )
  {
    int groupCount = this.config.getGroupCount();
    int compdata = 0;

    // to enable non contiguous channel groups
    // need to remove zero data from unused groups
    int indata = aSampleValue;
    for ( int j = 0, outcount = 0; j < groupCount; j++ )
    {
      if ( this.config.isGroupEnabled( j ) )
      {
        compdata |= ( ( indata & 0xff ) << ( 8 * outcount++ ) );
      }
      indata >>= 8;
    }
    return compdata;
  }
}
