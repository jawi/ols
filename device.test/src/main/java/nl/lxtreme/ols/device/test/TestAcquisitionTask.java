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
package nl.lxtreme.ols.device.test;


import static nl.lxtreme.ols.device.test.TestDeviceDialog.*;

import java.util.*;

import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;


/**
 * Denotes an acquisition task for a testing device that outputs static
 * (generated) data.
 */
public class TestAcquisitionTask implements AcquisitionTask
{
  // VARIABLES

  private final TestDeviceDialog configDialog;
  private final AcquisitionProgressListener progressListener;

  // CONSTRUCTORS

  /**
   * Creates a new TestDevice instance.
   * 
   * @param aConfigDialog
   * @param aProgressListener
   */
  public TestAcquisitionTask( final TestDeviceDialog aConfigDialog, final AcquisitionProgressListener aProgressListener )
  {
    this.configDialog = aConfigDialog;
    this.progressListener = aProgressListener;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult call() throws Exception
  {
    final String dataFunction = this.configDialog.getDataFunction();
    final int dataLength = this.configDialog.getDataLength();
    final int channels = this.configDialog.getChannels();

    final int[] data;
    int rate = 1000000000;

    final double halfWidth = ( 1L << ( channels - 1 ) ) / 2.0;

    if ( DATA_FUNCTIONS[6].equals( dataFunction ) )
    {
      final I2CGenerator generator = new I2CGenerator();
      generator.writeBitStream( "Hello World, this is a sample I2C bit stream!" );
      data = generator.getData();
      rate = generator.getRate();
    }
    else if ( DATA_FUNCTIONS[7].equals( dataFunction ) )
    {
      final OneWireGenerator generator = new OneWireGenerator( true /* aStandard */);
      generator.writeBitStream( "Hello World, this is a sample 1-wire bit stream!" );
      data = generator.getData();
      rate = generator.getRate();
    }
    else
    {
      final Random rnd = new Random();

      data = new int[dataLength];
      for ( int i = 0; i < data.length; i++ )
      {
        if ( DATA_FUNCTIONS[0].equals( dataFunction ) )
        {
          final int v = ( i / 8 ) & 0xff;
          data[i] = ( 255 - v ) | ( v << 8 ) | ( ( 255 - v ) << 16 ) | ( v << 24 );
        }
        else if ( DATA_FUNCTIONS[1].equals( dataFunction ) )
        {
          data[i] = 0x00;
        }
        else if ( DATA_FUNCTIONS[2].equals( dataFunction ) )
        {
          data[i] = ( int )( halfWidth + ( halfWidth * Math.sin( i / ( data.length / ( double )channels ) ) ) );
        }
        else if ( DATA_FUNCTIONS[3].equals( dataFunction ) )
        {
          data[i] = ( ( i % 2 ) == 0 ) ? 0x55 : 0xAA;
        }
        else if ( DATA_FUNCTIONS[4].equals( dataFunction ) )
        {
          data[i] = ( ( i % 4 ) == 0 ) ? 0x55 : 0xAA;
        }
        else if ( DATA_FUNCTIONS[5].equals( dataFunction ) )
        {
          data[i] = rnd.nextInt();
        }

        this.progressListener.acquisitionInProgress( ( int )( ( i * 100.0 ) / data.length ) );
      }
    }

    return new CapturedData( data, 23, rate, channels, Integer.MAX_VALUE );
  }
}
