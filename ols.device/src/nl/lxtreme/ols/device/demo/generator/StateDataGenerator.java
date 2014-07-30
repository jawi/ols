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
package nl.lxtreme.ols.device.demo.generator;


import java.util.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.demo.*;


/**
 * Provides a data generator that outputs state-data.
 */
public class StateDataGenerator implements DataGenerator
{
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
  public void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder, AcquisitionProgressListener aProgressListener )
  {
    int channelCount = Math.max( 16, aChannelCount );

    aBuilder.setChannelCount( channelCount );
    aBuilder.setSampleRate( OlsConstants.NOT_AVAILABLE );
    aBuilder.setTriggerPosition( 0 );

    aBuilder.add( aBuilder.createChannelGroup().setIndex( 1 ).setName( "Data" ).addChannel( 0, 1, 2, 3 ) ).add(
        aBuilder.createChannelGroup().setIndex( 0 ).setName( "Addr" )
            .addChannel( 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ) );

    Random rnd = new Random();

    int data = 0, addr = 0;
    for ( int i = 0; !Thread.currentThread().isInterrupted() && i < aSampleCount; i++ )
    {
      aBuilder.addSample( i, ( addr << 4 ) | data );

      addr = ( addr + 1 ) % 4096;
      data = rnd.nextInt( 16 );

      aProgressListener.acquisitionInProgress( i * 100 / aSampleCount );
    }
  }
}

/* EOF */
