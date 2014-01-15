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

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.task.execution.*;


/**
 * Provides a generic acquisition task that can read from any file-based source.
 */
public final class GenericDeviceAcquisitionTask implements Task<AcquisitionData>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( GenericDeviceAcquisitionTask.class.getName() );

  // VARIABLES

  private final String devicePath;
  private final int channelCount;
  private final int sampleRate;
  private final int sampleCount;
  private final int sampleWidth;
  private final AcquisitionProgressListener progressListener;

  private volatile InputStream inputStream;

  // CONSTRUCTORS

  /**
   * Creates a new GenericDevice instance.
   * 
   * @param aContext
   *          the bundle context to use;
   * @param aDeviceConfig
   *          the device configuration to use.
   */
  public GenericDeviceAcquisitionTask( String aDevicePath, int aChannelCount, int aSampleRate, int aSampleCount,
      int aSampleWidth, final AcquisitionProgressListener aProgressListener )
  {
    this.devicePath = aDevicePath;
    this.channelCount = aChannelCount;
    this.sampleRate = aSampleRate;
    this.sampleCount = aSampleCount;
    this.sampleWidth = aSampleWidth;
    this.progressListener = aProgressListener;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public AcquisitionData call() throws IOException
  {
    final int enabledChannels = ( int )( ( 1L << this.channelCount ) - 1L );

    final int count = this.sampleCount * this.sampleWidth;

    AcquisitionDataBuilder builder = new AcquisitionDataBuilder();
    builder.setEnabledChannelMask( enabledChannels );
    builder.setChannelCount( this.channelCount );
    builder.setSampleRate( this.sampleRate );

    this.inputStream = new FileInputStream( this.devicePath );

    try
    {
      int idx = 0;
      while ( !Thread.currentThread().isInterrupted() && ( idx < count ) )
      {
        final int sample = readSample( this.sampleWidth );

        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.log( Level.FINE, "Read: 0x{0}", Integer.toHexString( sample ) );
        }

        builder.addSample( idx, sample );

        // Update the progress...
        this.progressListener.acquisitionInProgress( ( int )( ( idx++ * 100.0 ) / count ) );
      }

      return builder.build();
    }
    catch ( IOException exception )
    {
      // Rethrow the caught exception...
      throw exception;
    }
    finally
    {
      try
      {
        if ( this.inputStream != null )
        {
          this.inputStream.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Generic Acquisition Task";
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
