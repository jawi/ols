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
    // Normalize the raw data into the sample data, as expected...
    int[] samples = reverseSamplesIfNeeded( normalizeSamples( aSampleData, aRawSampleCount ) );

    // Restart the progress bar...
    aListener.acquisitionInProgress( 0 );

    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setSampleRate( this.config.getSampleRate() );
    // Issue #98: use the *enabled* channel count, not the total channel
    // count...
    builder.setChannelCount( this.config.getEnabledChannelCount() );
    builder.setEnabledChannelMask( this.config.getEnabledChannelMask() );

    if ( this.config.isRleEnabled() )
    {
      addRleEncodedSamples( builder, samples, aListener );
    }
    else
    {
      addPlainSamples( builder, samples, aListener );
    }

    return builder.build();
  }

  /**
   * Adds a given array of sample data as non-RLE encoded samples to a given
   * {@link AcquisitionDataBuilder}.
   * 
   * @param aBuilder
   *          the builder to add the samples to;
   * @param aSamples
   *          the samples to add;
   * @param aListener
   *          the listener to report progress to.
   */
  private void addPlainSamples( AcquisitionDataBuilder aBuilder, int[] aSamples, AcquisitionProgressListener aListener )
  {
    if ( this.config.isTriggerEnabled() )
    {
      aBuilder.setTriggerPosition( this.config.getTriggerPosition() );
    }

    int length = aSamples.length;
    int blocks = Math.max( 1, length / 100 );

    long time = 0L;
    for ( int i = 0; i < length; i++ )
    {
      int sampleValue = aSamples[i];

      aBuilder.addSample( time++, sampleValue );

      if ( ( i % blocks ) == 0 )
      {
        aListener.acquisitionInProgress( ( i * 100 ) / length );
      }
    }
  }

  /**
   * Adds a given array of sample data as RLE encoded samples to a given
   * {@link AcquisitionDataBuilder}.
   * 
   * @param aBuilder
   *          the builder to add the samples to;
   * @param aSamples
   *          the RLE-encoded samples to add;
   * @param aListener
   *          the listener to report progress to.
   */
  private void addRleEncodedSamples( AcquisitionDataBuilder aBuilder, int[] aSamples,
      AcquisitionProgressListener aListener )
  {
    boolean ddrMode = this.config.isDoubleDataRateEnabled();

    boolean triggerSet = !this.config.isTriggerEnabled();
    int triggerPosition = this.config.getTriggerPosition();

    // enabled group count is "automatically" corrected for DDR/Demux mode...
    int width = 8 * this.config.getEnabledGroupCount();
    int rleCountValue = ( int )( 1L << ( width - 1 ) );
    int rleCountMask = rleCountValue - 1;

    int length = aSamples.length;
    int blocks = Math.max( 1, length / 100 );

    long time = 0L;
    for ( int i = 0; i < length; i++ )
    {
      int sampleValue = aSamples[i];
      int normalSampleValue = normalizeRleSample( sampleValue );

      if ( ( normalSampleValue & rleCountValue ) != 0 )
      {
        long count = ( normalSampleValue & rleCountMask );

        if ( ddrMode && ( i < ( aSamples.length - 1 ) ) )
        {
          // In case of "double data rate", the RLE-counts are encoded as 16-
          // resp. 32-bit values, so we need to take two samples for each
          // count (as they are 8- or 16-bits in DDR mode).
          // This should also solve issue #31...

          // Issue #55: double the RLE-count as we're using DDR mode which
          // takes two samples in one time period...
          long ddrCount = ( count << width ) | normalizeRleSample( aSamples[++i] );
          count = 2L * ddrCount;
        }

        if ( time > 0 )
        {
          time += count;
        }
        else
        {
          LOG.warning( "Ignoring RLE count without preceeding sample value @ " + Long.toString( count ) );
        }
      }
      else
      {
        if ( i >= triggerPosition && !triggerSet )
        {
          aBuilder.setTriggerPosition( time );
        }

        aBuilder.addSample( time, sampleValue );
        
        time++;
      }

      if ( ( i % blocks ) == 0 )
      {
        aListener.acquisitionInProgress( ( i * 100 ) / length );
      }
    }
  }

  /**
   * Normalizes the given sample value to mask out the unused channel groups and
   * get a sample value in the correct width.
   * 
   * @param aSampleValue
   *          the original sample to normalize.
   * @return the normalized sample value.
   */
  private int normalizeRleSample( int aSampleValue )
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

  /**
   * Normalizes the given raw sample data to mask out the unused channel groups
   * and get sample values in the correct width and format.
   * 
   * @param aSampleData
   * @param aRawSampleCount
   * @return the normalized sample data, never <code>null</code>.
   */
  private int[] normalizeSamples( byte[] aSampleData, int aRawSampleCount )
  {
    int groupCount = this.config.getGroupCount();

    int enabledGroupCount = this.config.getEnabledGroupCount();
    // Determine the number of samples read...
    int sampleCount = ( int )Math.floor( aRawSampleCount / ( double )enabledGroupCount );

    int[] samples = new int[sampleCount];
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
    return samples;
  }

  /**
   * Reverses the order of the given sample buffer, if needed according to our
   * configuration.
   * 
   * @param aSamples
   *          the samples to optionally reverse, cannot be <code>null</code>.
   * @return the given samples.
   */
  private int[] reverseSamplesIfNeeded( int[] aSamples )
  {
    if ( !this.config.isLastSampleSentFirst() )
    {
      return aSamples;
    }

    // Reverse all samples, as they are send backwards (original SUMP
    // protocol)...
    for ( int left = 0, right = aSamples.length - 1; left < right; left++, right-- )
    {
      // exchange the first and last
      int temp = aSamples[left];
      aSamples[left] = aSamples[right];
      aSamples[right] = temp;
    }

    return aSamples;
  }
}
