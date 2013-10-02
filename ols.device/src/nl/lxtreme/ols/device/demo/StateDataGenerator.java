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


import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a data generator that outputs state-data.
 */
final class StateDataGenerator implements IDataGenerator
{
  // CONSTANTS

  private static final int CLOCK = 0x01;

  // CONSTRUCTORS

  /**
   * Creates a new {@link StateDataGenerator} instance.
   */
  public StateDataGenerator()
  {
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "State data";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder,
      AcquisitionProgressListener aProgressListener )
  {
    int channelCount = Math.max( 4, aChannelCount );

    aBuilder.setChannelCount( channelCount );
    aBuilder.setSampleRate( OlsConstants.NOT_AVAILABLE );
    aBuilder.setTriggerPosition( 0 );

    int value = 0, x = 0;
    for ( int i = 0; i < aSampleCount; i++ )
    {
      if ( ( i % ( channelCount - 1 ) ) == 0 )
      {
        if ( ( value & CLOCK ) == 0 )
        {
          value = CLOCK | ( x++ << 1 );
        }
        else
        {
          value = ( x << 1 );
        }
      }

      aBuilder.addSample( i, value );

      aProgressListener.acquisitionInProgress( i * 100 / aSampleCount );
    }
  }
}

/* EOF */
