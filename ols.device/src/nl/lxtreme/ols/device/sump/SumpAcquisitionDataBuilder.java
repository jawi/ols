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

import java.util.logging.*;

import nl.lxtreme.ols.common.acquisition.*;


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

    if ( this.config.isTriggerEnabled() )
    {
      builder.setTriggerPosition( this.config.getTriggerPosition() );
    }

    final int groupCount = this.config.getGroupCount();

    // Restart the progress bar...
    aListener.acquisitionInProgress( 0 );

    // enabled group count is "automatically" corrected for DDR/Demux mode...
    final int width = getRLEDataWidth();
    int rleCountValue = ( int )( 1L << ( width - 1 ) );
    int rleCountMask = rleCountValue - 1;

    // Normalize the raw data into the sample data, as expected...
    int[] samples = new int[this.config.getSampleCount()];
    for ( int i = 0, j = 0; i < samples.length; i++ )
    {
      for ( int g = 0; g < groupCount; g++ )
      {
        if ( this.config.isGroupEnabled( g ) )
        {
          samples[i] |= ( ( aSampleData[j++] & 0xff ) << ( 8 * g ) );
        }
      }
    }

    if ( this.config.isLastSampleSentFirst() )
    {
      // Reverse all samples, as they are send backwards (original SUMP
      // protocol)...
      for ( int left = 0, right = samples.length - 1; left < right; left++, right-- )
      {
        // exchange the first and last
        int temp = samples[left];
        samples[left] = samples[right];
        samples[right] = temp;
      }
    }

    // Normalize the raw data into the sample data, as expected...
    long lastTime = 0L;
    long time = 0L;

    for ( int i = 0; i < samples.length; i++ )
    {
      int sampleValue = samples[i];

      if ( !this.config.isRleEnabled() )
      {
        builder.addSample( time++, sampleValue );
      }
      else
      {
        int normalSampleValue = normalizeSampleValue( sampleValue );

        if ( ( normalSampleValue & rleCountValue ) != 0 )
        {
          long count = ( normalSampleValue & rleCountMask );

          if ( this.config.isDoubleDataRateEnabled() && ( i < ( samples.length - 1 ) ) )
          {
            // In case of "double data rate", the RLE-counts are encoded as 16-
            // resp. 32-bit values, so we need to take two samples for each
            // count (as they are 8- or 16-bits in DDR mode).
            // This should also solve issue #31...

            // Issue #55: double the RLE-count as we're using DDR mode which
            // takes two samples in one time period...
            long ddrCount = ( count << width ) | normalizeSampleValue( samples[++i] );
            count = 2L * ddrCount;
          }

          if ( time > 0 )
          {
            time += count;
          }
        }
        else
        {
          // TODO this code needs some additional TLC...
          if ( lastTime > 0 && ( lastTime == time ) )
          {
            LOG.warning( String.format( "SPURIOUS SAMPLE @ %d (%d) : %d.%n", i, time, sampleValue ) );
            builder.addSample( ++time, sampleValue );
          }
          else
          {
            if ( time > 0 && lastTime == 0 )
            {
              // ensure we always start at time 0...
              builder.addSample( lastTime, sampleValue );
            }
            builder.addSample( time, sampleValue );
          }
          lastTime = time;
        }

        time++;
      }

      aListener.acquisitionInProgress( ( i * 100 ) / aSampleData.length );
    }

    return builder.build();
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
