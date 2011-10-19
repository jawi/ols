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
package nl.lxtreme.ols.device.generic;


import java.io.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a generic device that can read from any file-based source.
 */
public final class GenericDevice implements Device
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( GenericDevice.class.getName() );

  // VARIABLES

  private final AcquisitionProgressListener progressListener;
  private final GenericDeviceConfigDialog deviceConfig;

  private InputStream inputStream;

  // CONSTRUCTORS

  /**
   * Creates a new GenericDevice instance.
   * 
   * @param aContext
   *          the bundle context to use;
   * @param aDeviceConfig
   *          the device configuration to use.
   */
  public GenericDevice( final GenericDeviceConfigDialog aDeviceConfig,
      final AcquisitionProgressListener aProgressListener )
  {
    this.deviceConfig = aDeviceConfig;
    this.progressListener = aProgressListener;

  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionResult call() throws IOException
  {
    final int width = this.deviceConfig.getSampleWidth();
    final int depth = this.deviceConfig.getSampleDepth();
    final int rate = this.deviceConfig.getSampleRate();
    final int channels = this.deviceConfig.getChannelCount();

    final int count = depth * width;

    final int[] values = new int[count];
    final long[] timestamps = new long[count];

    try
    {
      int idx = 0;
      while ( !Thread.currentThread().isInterrupted() && ( idx < count ) )
      {
        final int sample = readSample( width );

        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.log( Level.FINE, "Read: 0x{0}", Integer.toHexString( sample ) );
        }

        values[idx] = sample;
        timestamps[idx] = idx;

        // Update the progress...
        this.progressListener.acquisitionInProgress( ( int )( ( idx++ * 100.0 ) / count ) );
      }

      return new CapturedData( values, timestamps, Ols.NOT_AVAILABLE, rate, channels, channels, idx );
    }
    catch ( IOException exception )
    {
      // Rethrow the caught exception...
      throw exception;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    HostUtils.closeResource( this.inputStream );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void open() throws IOException
  {
    this.inputStream = new FileInputStream( this.deviceConfig.getDevicePath() );
  }

  /**
   * Reads <code>channels</code> / 8 bytes from stream and compiles them into a
   * single integer.
   * 
   * @param aChannelCount
   *          number of channels to read (must be multiple of 8)
   * @return integer containing four bytes read
   * @throws IOException
   *           if stream reading fails
   */
  private int readSample( final int aSampleWidth ) throws IOException
  {
    int v, value = 0;

    for ( int i = 0; !Thread.currentThread().isInterrupted() && ( i < aSampleWidth ); i++ )
    {
      v = this.inputStream.read();

      // Any timeouts/interrupts occurred?
      if ( v < 0 )
      {
        throw new EOFException( "Data readout interrupted: EOF." );
      }

      value |= v << ( 8 * i );
    }

    return value;
  }

}
