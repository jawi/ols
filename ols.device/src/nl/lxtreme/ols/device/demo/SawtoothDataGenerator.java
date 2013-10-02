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
package nl.lxtreme.ols.device.demo;


import nl.lxtreme.ols.common.acquisition.*;


/**
 * Provides a sawtooth-generator.
 */
final class SawtoothDataGenerator implements IDataGenerator
{
  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Sawtooth";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void generate( int aChannelCount, int aSampleCount, AcquisitionDataBuilder aBuilder,
      AcquisitionProgressListener aProgressListener )
  {
    aBuilder.setChannelCount( 32 );
    aBuilder.setTriggerPosition( ( int )( aSampleCount * 0.25 ) );

    for ( int i = 0; i < aSampleCount; i++ )
    {
      final int v = ( i / 8 ) & 0xff;
      aBuilder.addSample( i, ( 255 - v ) | ( v << 8 ) | ( ( 255 - v ) << 16 ) | ( v << 24 ) );

      aProgressListener.acquisitionInProgress( i * 100 / aSampleCount );
    }
  }
}
