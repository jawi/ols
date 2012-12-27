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
import java.nio.*;
import java.nio.channels.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.ioutil.*;
import aQute.bnd.annotation.metatype.*;


/**
 * Provides a generic acquisition task that can read from any file-based source.
 */
public final class GenericDeviceAcquisitionTask implements Callable<AcquisitionData>
{
  // INNER TYPES

  /**
   * Provides a in-memory {@link WritableByteChannel} that writes all data to a
   * byte-array.
   */
  static class MemoryWritableByteBuffer implements WritableByteChannel
  {
    // VARIABLES

    private final ByteBuffer buffer;
    private final byte[] data;
    private volatile boolean open = true;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MemoryWritableByteBuffer} instance.
     */
    public MemoryWritableByteBuffer( final int size )
    {
      this.data = new byte[size];
      this.buffer = ByteBuffer.wrap( this.data );
    }

    // METHODS

    /**
     * Returns the current value of array.
     * 
     * @return the array
     */
    public byte[] getData()
    {
      return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isOpen()
    {
      return this.open;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() throws IOException
    {
      this.open = false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int write( final ByteBuffer aSrc ) throws IOException
    {
      int pos = this.buffer.position();
      this.buffer.put( aSrc );
      return this.buffer.position() - pos;
    }
  }

  // VARIABLES

  private final DeviceProgressListener progressListener;
  private final GenericConfig configuration;

  // CONSTRUCTORS

  /**
   * Creates a new {@link GenericDeviceAcquisitionTask} instance.
   * 
   * @param aConfiguration
   *          the device configuration to use;
   * @param aProgressListener
   *          the callback to report the progress to.
   */
  public GenericDeviceAcquisitionTask( final Configuration aConfiguration,
      final DeviceProgressListener aProgressListener )
  {
    this.configuration = Configurable.createConfigurable( GenericConfig.class, aConfiguration.asMap() );
    this.progressListener = aProgressListener;

  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "resource" )
  public AcquisitionData call() throws IOException
  {
    FileChannel channel = new FileInputStream( this.configuration.path() ).getChannel();

    final int width = this.configuration.sampleWidth(); // in bytes
    final int depth = this.configuration.sampleDepth(); // #
    final int size = depth * width; // total # of bytes to read...

    MemoryWritableByteBuffer target = new MemoryWritableByteBuffer( size );

    try
    {
      int offset = 0;
      int byteCount = size;
      while ( !Thread.currentThread().isInterrupted() && ( byteCount > 0 ) )
      {
        // Using this NIO construct allows us to have non-blocking I/O; making
        // it possible to interrupt the acquisition from this device...
        long read = channel.transferTo( offset, Math.max( 1, channel.size() ), target );
        if ( read < 0 )
        {
          channel.close();

          throw new EOFException( "Only " + offset + " bytes read!" );
        }
        else
        {
          byteCount -= read;
          offset += read;
        }

        // Update the progress...
        this.progressListener.acquisitionInProgress( ( 100 * offset ) / size );
      }

      // Build the resulting acquisition data...
      AcquisitionDataBuilder builder = new AcquisitionDataBuilder() //
          .setTriggerPosition( 0L ) //
          .setSampleRate( this.configuration.sampleRate() ) //
          .setChannelCount( this.configuration.channelCount() );

      // Normalize sample data...
      for ( int i = 0, j = 0; i < depth; i++ )
      {
        int sample = 0;
        for ( int k = 0; k < width; k++ )
        {
          sample |= ( target.data[j++] << ( 8 * k ) );
        }
        builder.addSample( i, sample );
      }

      return builder.build();
    }
    finally
    {
      IOUtil.closeResource( channel );
    }
  }
}
