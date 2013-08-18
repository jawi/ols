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


import org.sump.device.logicsniffer.*;


/**
 * Processes all samples and only returns the actual changed sample values.
 */
public final class EqualityFilter implements SampleProcessor
{
  // VARIABLES

  private final LogicSnifferConfig config;
  private final int[] buffer;
  private final int trigCount;
  private final SampleProcessorCallback callback;

  // CONSTRUCTORS

  /**
   * @param aConfig
   *          the configuration to use;
   * @param aBuffer
   *          the buffer with sample data to decode.
   * @param aTrigCount
   *          the trigcount value;
   * @param aCallback
   *          the callback to use.
   */
  public EqualityFilter( final LogicSnifferConfig aConfig, final int[] aBuffer, final int aTrigCount,
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
  }

  // METHODS

  /**
   * @see org.sump.device.logicsniffer.sampleprocessor.SampleProcessor#process()
   */
  @Override
  public final void process()
  {
    long time = 0;

    final int samples = this.buffer.length;

    int lastSample = 0; // first value doesn't really matter
    for ( int i = 0; i < samples; i++ )
    {
      final int newSample = this.buffer[i];

      if ( ( i == 0 ) || ( lastSample != newSample ) )
      {
        // add the read sample & add a timestamp value as well...
        this.callback.addValue( newSample, time );
      }

      lastSample = newSample;
      time++;
    }

    // Ensure the last sample is shown as well (even if there was a lot of time
    // between the last real sample and the end of the capture; i.e., constant
    // data)...
    this.callback.addValue( lastSample, time );

    // XXX JaWi: why is this correction needed?
    int correction = 2;
    if ( this.config.getDivider() <= 3 )
    {
      correction = 1;
    }

    // Take the last seen time value as "absolete" length of this trace...
    this.callback.ready( time, ( this.trigCount - correction ) );
  }
}
