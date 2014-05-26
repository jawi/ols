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
package nl.lxtreme.ols.device.sump;


import java.util.logging.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.sump.config.*;


/**
 * Custom {@link AcquisitionData} builder that keeps the configuration of a
 * SUMP-compatible device into consideration.
 */
public class SumpAcquisitionDataBuilder
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SumpAcquisitionDataBuilder.class.getName() );

  // VARIABLES

  private final SumpConfig config;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpAcquisitionDataBuilder} instance.
   */
  public SumpAcquisitionDataBuilder( SumpConfig aConfig )
  {
    this.config = aConfig;
  }

  // METHODS

  /**
   * Builds the {@link AcquisitionData} based on the given "raw" sample data
   * read from the SUMP device.
   * 
   * @param aSampleData
   *          the raw sample data to process;
   * @param aRawSampleCount
   *          the number of samples read as given in the sample data, >= 0;
   * @param aListener
   *          the progress listener to use.
   * @return a new {@link AcquisitionData} containing the processed sample data,
   *         never <code>null</code>.
   */
  public AcquisitionData build( byte[] aSampleData, int aRawSampleCount, AcquisitionProgressListener aListener )
  {
    final int groupCount = this.config.getGroupCount();
    final int enabledGroupsMask = ( ~this.config.getFlags() >> 2 ) & 0x0f;
    final int enabledGroupCount = Integer.bitCount( enabledGroupsMask );
    final boolean ddrMode = this.config.isDoubleDataRateEnabled();
    final boolean rleMode = this.config.isRleEnabled();
    final int rleCountValue = ( int )( 1L << ( ( 8 * enabledGroupCount ) - 1 ) );
    final int rleCountMask = rleCountValue - 1;

    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setReverseSampleOrder( this.config.isLastSampleSentFirst() );
    builder.setSampleRate( this.config.getSampleRate() );
    builder.setTriggerPosition( this.config.getTriggerPosition() );
    // Issue #98: use the *enabled* channel count, not the total channel
    // count...
    builder.setChannelCount( 8 * enabledGroupCount );
    builder.setEnabledChannelMask( this.config.getEnabledChannelMask() );

    int blocks = ( int )Math.max( 1, Math.ceil( aRawSampleCount / 100.0 ) );
    int i = 0, lastValue = 0, count, progress = 0;
    long timestamp = 0L;

    // Process the raw sample data:
    while ( i < aRawSampleCount )
    {
      // 1. normalize the raw bytes into (non-aligned) sample values;
      int value = 0;
      for ( int g = 0; g < enabledGroupCount; g++ )
      {
        value <<= 8;
        value |= aSampleData[i++] & 0xff;
      }

      // 2. (optionally) decode RLE encoded values; note that this should work
      // for all "extra" RLE modes, supported by the DemonCore (derived)
      // firmwares, as well...
      count = 1;
      if ( rleMode && ( ( value & rleCountValue ) != 0 ) )
      {
        if ( timestamp == 0 )
        {
          // RLE count seen as first value?! This shouldn't happen...
          LOG.warning( "Ignoring RLE count without preceeding sample value @ " + Long.toString( timestamp ) );
          continue;
        }
        // RLE count value, simply add the last sample value X-1 times...
        count = ( value & rleCountMask ) - 1;
        value = lastValue;
      }

      // Keep for next iteration, use the *non-aligned* value to ensure the
      // aligning step remains working properly...
      lastValue = value;

      // 3. align the sample value;
      if ( enabledGroupCount != groupCount )
      {
        int newValue = 0;
        for ( int g = 0; g < groupCount; g++ )
        {
          if ( ( enabledGroupsMask & ( 1 << g ) ) != 0 )
          {
            newValue |= ( ( value & 0xff ) << ( 8 * g ) );
            value >>>= 8;
          }
        }
        value = newValue;
      }

      // 4. add the actual sample...
      if ( ddrMode )
      {
        // In case RLE is *enabled*, we need to add the (last) sample "count"
        // times, in case RLE is *disabled* we simply add it once...
        while ( count-- > 0 )
        {
          // 4a. (optionally) split and add packed/DDR/mux'd samples
          builder.addSample( timestamp++, ( value >>> 16 ) & 0xFFFF );
          builder.addSample( timestamp++, value & 0xFFFF );
        }
      }
      else
      {
        // 4b. add the sample to our builder
        builder.addSample( timestamp, value );
        // In case RLE is *enabled* we need to add the last sample "count"
        // times, which is equivalent to incrementing our timestamp...
        timestamp += count;
      }

      if ( ( i % blocks ) == 0 )
      {
        progress = ( i * 100 ) / aRawSampleCount;
        aListener.acquisitionInProgress( progress );
      }
    }

    // We know the last timestamp now, make it available to the builder...
    builder.setAbsoluteLength( timestamp - 1 );

    // Ensure we always "finalize" the progress...
    if ( progress != 100 )
    {
      aListener.acquisitionInProgress( 100 );
    }

    return builder.build();
  }
}
