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
package nl.lxtreme.ols.device.demo.generator;


import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.demo.*;


/**
 * Provides a sine-generator.
 */
public class SineDataGenerator implements DataGenerator
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Sine";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder,
      AcquisitionProgressListener aProgressListener )
  {
    int channelCount = Math.min( 16, aChannelCount );

    aBuilder.setChannelCount( aChannelCount );
    aBuilder.setSampleRate( SR_10MHZ );
    aBuilder.setTriggerPosition( ( int )( aSampleCount * 0.25 ) );

    // Make a single group with all channels...
    ChannelGroupBuilder group = aBuilder.createChannelGroup().setIndex( 0 ).setName( "Sinus" );
    for ( int i = 0; i < aChannelCount; i++ )
    {
      group.addChannel( i );
    }
    aBuilder.add( group );

    final double max = ( ( ( 1L << channelCount ) - 1L ) & 0xFFFFFFFFL );
    final double half = ( max / 2.0 );
    final double factor = ( ( 2.0 * Math.PI ) / max );

    for ( int i = 0; !Thread.currentThread().isInterrupted() && i < aSampleCount; i++ )
    {
      aBuilder.addSample( i, ( int )( half + ( half * Math.sin( i * factor ) ) ) );

      aProgressListener.acquisitionInProgress( i * 100 / aSampleCount );
    }
  }
}
