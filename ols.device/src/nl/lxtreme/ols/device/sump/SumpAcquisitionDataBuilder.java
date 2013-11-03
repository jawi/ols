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


import static nl.lxtreme.ols.device.sump.SumpConstants.*;

import java.util.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Custom {@link AcquisitionData} builder that keeps the configuration of a
 * SUMP-compatible device into consideration.
 */
public class SumpAcquisitionDataBuilder
{
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
   * @param aListener
   *          the progress listener to use.
   * @return a new {@link AcquisitionData} containing the processed sample data,
   *         never <code>null</code>.
   */
  public AcquisitionData build( byte[] aSampleData, AcquisitionProgressListener aListener )
  {
    final AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setSampleRate( this.config.getSampleRate() );
    // Issue #98: use the *enabled* channel count, not the total channel
    // count...
    builder.setChannelCount( this.config.getEnabledChannelCount() );
    builder.setEnabledChannelMask( this.config.getEnabledChannelMask() );

    final int groupCount = this.config.getGroupCount();
    final int sampleCount = this.config.getSampleCount();
    final boolean samplesInReverseOrder = this.config.isSamplesInReverseOrder();

    // get the "raw" value of the enabled groups, in case of DDR-mode, this will
    // provide us with the correct groups...
    final int enabledGroups = ( ~this.config.getFlags() >> 2 ) & 0xF;

    // Restart the progress bar...
    aListener.acquisitionInProgress( 0 );

    // enabled group count is "automatically" corrected for DDR/Demux mode...
    final int width = getRLEDataWidth();
    int rleCountValue = ( int )( 1L << ( width - 1 ) );
    int rleCountMask = rleCountValue - 1;

    // Normalize the raw data into the sample data, as expected...
    int j = 0;
    int lastSampleValue = 0;
    long time = 0L;
    for ( Integer idxValue : getIterator( sampleCount, samplesInReverseOrder ) )
    {
      int i = idxValue.intValue();
      if ( j == sampleCount )
      {
        break; // XXX
      }

      int sampleValue = 0;
      for ( int g = 0; g < groupCount; g++ )
      {
        if ( ( enabledGroups & ( 1 << g ) ) != 0 )
        {
          sampleValue |= ( aSampleData[j++] & 0xff ) << ( g * 8 );
        }
      }

      if ( this.config.isTriggerEnabled() && ( j == this.config.getTriggerPosition() ) )
      {
        builder.setTriggerPosition( time + 1 );
      }

      if ( this.config.isRleEnabled() )
      {
        if ( ( sampleValue & rleCountValue ) != 0 )
        {
          long count = ( sampleValue & rleCountMask );
          sampleValue = lastSampleValue;

          if ( this.config.isDoubleDataRateEnabled() && ( i < ( sampleCount - 1 ) ) )
          {
            // In case of "double data rate", the RLE-counts are encoded as 16-
            // resp. 32-bit values, so we need to take two samples for each
            // count (as they are 8- or 16-bits in DDR mode).
            // This should also solve issue #31...

            // Issue #55: double the RLE-count as we're using DDR mode which
            // takes two samples in one time period...
            long ddrCount = ( count << width ); /*
                                                 * | normalizeSampleValue(
                                                 * this.buffer[++i] ) );
                                                 */// XXX
            count = 2L * ddrCount;
          }

          time += count;
        }

        builder.addSample( time++, sampleValue );
        lastSampleValue = sampleValue;
      }
      else
      {
        builder.addSample( time++, sampleValue );
      }

      aListener.acquisitionInProgress( ( j * 100 ) / aSampleData.length );
    }

    return builder.build();
  }

  /**
   * Creates an {@link Iterator} that can either count forward from zero to a
   * given maximum, or backward from a given maximum to zero.
   * 
   * @param aMax
   * @param aCountForward
   * @return an {@link Iterable}, never <code>null</code>.
   */
  final Iterable<Integer> getIterator( final int aMax, final boolean aCountForward )
  {
    return new Iterable<Integer>()
    {
      final int start = aCountForward ? 0 : aMax;
      final int end = aCountForward ? aMax : 0;

      @Override
      public Iterator<Integer> iterator()
      {
        return new Iterator<Integer>()
        {
          private volatile int idx = start;

          @Override
          public boolean hasNext()
          {
            return aCountForward ? ( idx < end ) : ( idx > end );
          }

          @Override
          public Integer next()
          {
            Integer result = Integer.valueOf( idx );
            idx += aCountForward ? 1 : -1;
            return result;
          }

          @Override
          public void remove()
          {
            throw new UnsupportedOperationException();
          }
        };
      }
    };
  }

  /**
   * @return the data width, in bits.
   */
  private int getRLEDataWidth()
  {
    int enabledGroups = this.config.getEnabledGroupCount();
    if ( this.config.isDoubleDataRateEnabled() )
    {
      Math.min( MAX_CHANNEL_GROUPS_DDR, enabledGroups );
    }
    return enabledGroups * 8;
  }
}
