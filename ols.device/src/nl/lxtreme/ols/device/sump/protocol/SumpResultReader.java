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
package nl.lxtreme.ols.device.sump.protocol;


import java.io.*;

import org.slf4j.*;

import nl.lxtreme.ols.device.sump.*;


/**
 * Wrapper to read SUMP-specific results from a normal {@link DataInputStream}.
 */
public class SumpResultReader implements Closeable, SumpProtocolConstants
{
  // CONSTANTS

  private static final Logger LOG = LoggerFactory.getLogger( SumpResultReader.class.getName() );

  // VARIABLES

  private final DataInputStream inputStream;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpResultReader} instance.
   * 
   * @param aInputStream
   *          the {@link DataInputStream} to read from, cannot be
   *          <code>null</code>.
   */
  public SumpResultReader( final DataInputStream aInputStream )
  {
    this.inputStream = aInputStream;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    this.inputStream.close();
  }

  /**
   * @return the found device ID, or -1 if no suitable device ID was found.
   * @throws IOException
   */
  public int readDeviceId() throws IOException
  {
    int id = this.inputStream.readInt();

    if ( id == SLA_V0 )
    {
      LOG.info( "Found (unsupported!) Sump Logic Analyzer ({})...", Integer.toHexString( id ) );
    }
    else if ( id == SLA_V1 )
    {
      LOG.info( "Found Sump Logic Analyzer/LogicSniffer compatible device ..." );
    }
    else
    {
      LOG.info( "Found unknown device: 0x{} ...", Integer.toHexString( id ) );
      id = -1;
    }
    return id;
  }

  /**
   * Tries to obtain the OLS device's metadata.
   * 
   * @return the device metadata, can be not populated, but never
   *         <code>null</code>.
   * @throws IOException
   *           in case of I/O problems;
   * @throws IllegalStateException
   *           in case we're not attached to the OLS device.
   */
  public boolean readMetadata( final DeviceMetadata aMetadata ) throws IOException, IllegalStateException
  {
    boolean gotResponse = false;

    int result = -1;
    do
    {
      try
      {
        result = this.inputStream.read();

        if ( result > 0 )
        {
          // We've got response!
          gotResponse = true;

          final int type = ( result & 0xE0 ) >> 5;
          if ( type == 0x00 )
          {
            // key value is a null-terminated string...
            final String value = readString();
            aMetadata.add( result, value );
          }
          else if ( type == 0x01 )
          {
            // key value is a 32-bit integer; least significant byte first...
            // final Integer value = NumberUtils.convertByteOrder(
            // this.inputStream.readInt(), 32, ByteOrder.LITTLE_ENDIAN );
            final int value = this.inputStream.readInt();
            aMetadata.add( result, Integer.valueOf( value ) );
          }
          else if ( type == 0x02 )
          {
            // key value is a 8-bit integer...
            final int value = this.inputStream.read();
            aMetadata.add( result, Integer.valueOf( value ) );
          }
          else
          {
            LOG.info( "Ignoring unknown metadata type: {}", Integer.valueOf( type ) );
          }
        }
      }
      catch ( final IOException exception )
      {
        /* don't care */
        result = -1;

        // Make sure to handle IO-interrupted exceptions properly!
        if ( exception instanceof InterruptedIOException )
        {
          Thread.currentThread().interrupt();
        }
        else
        {
          LOG.warn( "I/O exception", exception );
        }
      }
    }
    while ( ( result > 0x00 ) && !Thread.currentThread().isInterrupted() );

    return gotResponse;
  }

  /**
   * Reads raw data from the contained input stream, non blocking
   * 
   * @return the integer sample value containing up to four read bytes, not
   *         aligned.
   * @throws IOException
   *           if stream reading fails.
   */
  public int readRawData( byte[] aBuffer, int aOffset, int aCount ) throws IOException
  {
    int availableCount = this.inputStream.available();
    if ( availableCount <= 0 )
    {
      return availableCount;
    }
    if ( availableCount > aCount )
    {
      availableCount = aCount;
    }
    return this.inputStream.read( aBuffer, aOffset, availableCount );
  }

  /**
   * Reads a zero-terminated ASCII-string from the current input stream.
   * 
   * @return the read string, can be empty but never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the string read.
   */
  final String readString() throws IOException
  {
    StringBuilder sb = new StringBuilder();

    int read = -1;
    do
    {
      read = this.inputStream.read();
      if ( read > 0x00 )
      {
        // no additional conversion to UTF-8 is needed, as the ASCII character
        // set is a subset of UTF-8...
        sb.append( ( char )read );
      }
    }
    while ( ( read > 0x00 ) && !Thread.currentThread().isInterrupted() );

    return sb.toString();
  }
}
