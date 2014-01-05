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
final class BurstGenerator implements IDataGenerator
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link BurstGenerator} instance.
   */
  public BurstGenerator()
  {
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Burst data";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder,
      AcquisitionProgressListener aProgressListener )
  {
    int channelCount = Math.max( 4, aChannelCount );

    int counterSize = channelCount - 1;
    int counterMax = ( 1 << counterSize ) - 1;
    int clockMask = ( 1 << counterSize );

    aBuilder.setChannelCount( channelCount );
    aBuilder.setSampleRate( SR_1MHZ );
    aBuilder.setTriggerPosition( 0 );

    // Make a single group with all channels...
    ChannelGroupBuilder group = aBuilder.createChannelGroup().setIndex( 0 ).setName( "Demo burst" );
    for ( int i = 0; i < aChannelCount; i++ )
    {
      group.addChannel( i );
    }
    aBuilder.add( group );

    int burstLength = ( aSampleCount / 5 );

    int value = 0, counter = 0, dir = -1, cursor = 0;
    long ts = 0;

    for ( int i = 0; i < aSampleCount; i++, ts++ )
    {
      if ( ( value & clockMask ) == 0 )
      {
        value = clockMask | counter;

        if ( counter == 0 || counter == counterMax )
        {
          dir = ( dir > 0 ) ? -1 : +1;
        }
        counter += dir;
      }
      else
      {
        value = counter;
      }

      if ( ( i > 0 ) && ( i % burstLength ) == 0 )
      {
        aBuilder.add( aBuilder.createCursor().setIndex( cursor++ ).setTimestamp( ts ) );
        ts += burstLength;
      }

      aBuilder.addSample( ts, value );

      aProgressListener.acquisitionInProgress( i * 100 / aSampleCount );
    }
  }
}

/* EOF */
