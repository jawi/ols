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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.device.demo;


import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a data generator that outputs state-data, with a clock on the last
 * channel.
 */
final class UnityGenerator implements IDataGenerator
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link UnityGenerator} instance.
   */
  public UnityGenerator()
  {
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Unity";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder,
      AcquisitionProgressListener aProgressListener )
  {
    int sampleCount = aChannelCount * aChannelCount;

    aBuilder.setChannelCount( aChannelCount );
    aBuilder.setSampleRate( 1 );
    aBuilder.setTriggerPosition( 10 );

    // Make a single group with all channels...
    ChannelGroupBuilder group = aBuilder.createChannelGroup().setIndex( 0 ).setName( "Unity" );
    for ( int i = 0; i < aChannelCount; i++ )
    {
      group.addChannel( i );
    }
    aBuilder.add( group );

    int value = 0;
    long ts = 0;

    for ( int i = 0; !Thread.currentThread().isInterrupted() && i < sampleCount; i++, ts += 10 )
    {
      aBuilder.addSample( ts, value );

      if ( i == 0 )
      {
        value = 1;
      }
      else if ( i < aChannelCount )
      {
        value <<= 1;
      }
      else if ( i == ( sampleCount - ( 2 * aChannelCount ) ) )
      {
        value = ( 1 << aChannelCount );
      }
      else if ( ( sampleCount - aChannelCount - i ) < aChannelCount )
      {
        value >>>= 1;
      }
      else
      {
        value = 0;
      }

      aProgressListener.acquisitionInProgress( i * 100 / aSampleCount );
    }
  }
}

/* EOF */
