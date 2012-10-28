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
package nl.lxtreme.ols.device.demo;


import static nl.lxtreme.ols.device.demo.DemoDeviceDialog.*;

import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;


/**
 * Denotes an acquisition task for a testing device that outputs static
 * (generated) data.
 */
public class DemoAcquisitionTask implements Callable<AcquisitionData>
{
  // VARIABLES

  private final DemoDeviceDialog configDialog;
  private final DeviceProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new TestDevice instance.
   * 
   * @param aConfigDialog
   * @param aProgressListener
   */
  public DemoAcquisitionTask( final DemoDeviceDialog aConfigDialog, final DeviceProgressListener aProgressListener )
  {
    this.configDialog = aConfigDialog;
    this.progressListener = aProgressListener;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData call()
  {
    final String dataFunction = this.configDialog.getDataFunction();
    final int dataLength = this.configDialog.getDataLength();
    final int channels = this.configDialog.getChannels();

    final AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setChannelCount( channels );

    final double max = ( ( ( 1L << Math.min( 16, channels ) ) - 1L ) & 0xFFFFFFFFL );
    final double half = ( max / 2.0 );
    final double factor = ( ( 2.0 * Math.PI ) / max );

    if ( DATA_FUNCTIONS[6].equals( dataFunction ) )
    {
      final I2CGenerator generator = new I2CGenerator();
      generator.writeBitStream( "Hello World, this is a sample I2C bit stream!" );
      generator.generate( builder );
    }
    else if ( DATA_FUNCTIONS[7].equals( dataFunction ) )
    {
      final OneWireGenerator generator = new OneWireGenerator( true /* aStandard */);
      generator.writeBitStream( "Hello World, this is a sample 1-wire bit stream!" );
      generator.generate( builder );
    }
    else if ( DATA_FUNCTIONS[8].equals( dataFunction ) )
    {
      final ManchesterEncoder encoder = new ManchesterEncoder();
      encoder.writeData( "Hello World!" );
      encoder.generate( builder );
    }
    else
    {
      final Random rnd = new Random();

      boolean state = false;

      for ( int i = 0; i < dataLength; i++ )
      {
        if ( DATA_FUNCTIONS[0].equals( dataFunction ) )
        {
          final int v = ( i / 8 ) & 0xff;
          final int sample = ( 255 - v ) | ( v << 8 ) | ( ( 255 - v ) << 16 ) | ( v << 24 );

          builder.addSample( i, sample );
        }
        else if ( DATA_FUNCTIONS[1].equals( dataFunction ) )
        {
          builder.addSample( i, 0x00 );
        }
        else if ( DATA_FUNCTIONS[2].equals( dataFunction ) )
        {
          int sample = ( int )( half + ( half * Math.sin( i * factor ) ) );
          builder.addSample( i, sample );
          builder.clearSampleRate();
        }
        else if ( DATA_FUNCTIONS[3].equals( dataFunction ) )
        {
          int sample = ( ( i % 2 ) == 0 ) ? 0x55 : 0xAA;
          builder.addSample( i, sample );
        }
        else if ( DATA_FUNCTIONS[4].equals( dataFunction ) )
        {
          int sample = ( ( i % 4 ) == 0 ) ? 0x55 : 0xAA;
          builder.addSample( i, sample );
        }
        else if ( DATA_FUNCTIONS[5].equals( dataFunction ) )
        {
          int sample = rnd.nextInt();
          builder.addSample( i, sample );
        }
        else if ( DATA_FUNCTIONS[9].equals( dataFunction ) )
        {
          if ( ( i % 4 ) == 0 )
          {
            state = !state;
          }
          int sample = state ? 0x00 : 0x8000;
          builder.addSample( i, sample );
          builder.setEnabledChannelMask( 0xFF00 );
        }

        this.progressListener.acquisitionInProgress( ( int )( ( i * 100.0 ) / dataLength ) );
      }

      builder.setTriggerPosition( ( int )( dataLength * 0.25 ) );
    }

    return builder.build();
  }
}
