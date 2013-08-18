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
package org.sump.device.logicsniffer;


import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.microedition.io.*;

import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.protocol.*;

import nl.lxtreme.ols.api.task.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides a task for detecting the current type of the attached logic sniffer
 * device.
 */
public class LogicSnifferDetectionTask implements Task<LogicSnifferMetadata>, SumpProtocolConstants
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( LogicSnifferDetectionTask.class.getName() );

  // VARIABLES

  private final LogicSnifferDevice device;
  private final String connectionURI;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDetectionTask instance.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  public LogicSnifferDetectionTask( final LogicSnifferDevice aDevice, final String aConnectionURI )
  {
    this.device = aDevice;
    this.connectionURI = aConnectionURI;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public LogicSnifferMetadata call() throws IOException
  {
    DataInputStream inputStream = null;
    DataOutputStream outputStream = null;
    StreamConnection connection = null;

    boolean gotResponse = false;

    try
    {
      connection = this.device.createStreamConnection( this.connectionURI );

      inputStream = connection.openDataInputStream();
      outputStream = connection.openDataOutputStream();

      final LogicSnifferMetadata metadata = new LogicSnifferMetadata();
      int tries = 3;

      do
      {
        // Make sure nothing is left in our input buffer...
        flushInputStream( inputStream );

        writeCmdReset( outputStream );

        // Force the device into SUMP mode; this is necessary for multi-purpose
        // devices as the IRToy, and BusPirate...
        writeCmdGetDeviceId( outputStream );
        readDeviceId( inputStream );

        LOG.log( Level.INFO, "Detected SUMP-compatible device ..." );

        // Make sure nothing is left in our input buffer...
        flushInputStream( inputStream );

        // Ok; device appears to be good and willing to communicate;
        // let's get its metadata...
        writeCmdGetMetadata( outputStream );

        if ( gotResponse = readMetadata( inputStream, metadata ) )
        {
          // Log the read results...
          LOG.log( Level.INFO, "Found device type: {0}", metadata.getName() );
          LOG.log( Level.FINE, "Device metadata = \n{0}", metadata.toString() );

          // Determine the device profile based on the information of the
          // metadata; it will be placed in the given metadata object...
          metadata.setDeviceProfile( getDeviceProfile( metadata.getName() ) );
        }
      }
      while ( !Thread.currentThread().isInterrupted() && !gotResponse && ( tries-- > 0 ) );

      return metadata;
    }
    finally
    {
      if ( outputStream != null )
      {
        // Reset the device again; this ensures correct working for devices
        // whose firmware do not understand the metadata command...
        writeCmdReset( outputStream );
      }

      HostUtils.closeResource( inputStream );
      HostUtils.closeResource( outputStream );

      try
      {
        if ( connection != null )
        {
          connection.close();
        }
      }
      catch ( IOException exception )
      {
        LOG.log( Level.INFO, "I/O connection while trying to close connection after device detect!", exception );
      }
    }
  }

  /**
   * Flushes the given input stream by reading as many bytes as there are still
   * available.
   * 
   * @param aResource
   *          the resource to flush, can be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems/
   */
  private void flushInputStream( final InputStream aResource ) throws IOException
  {
    if ( aResource != null )
    {
      while ( ( aResource.available() > 0 ) && ( aResource.read() >= 0 ) )
      {
      }
    }
  }

  /**
   * Determines the device profile for the current attached device. The device
   * profile provides us with detailed information about the capabilities of a
   * certain SUMP-compatible device.
   * 
   * @param aName
   *          the name of the device, can be <code>null</code>.
   * @return a device profile, or <code>null</code> if no such profile could be
   *         determined.
   */
  private DeviceProfile getDeviceProfile( final String aName )
  {
    DeviceProfile profile = null;

    if ( aName != null )
    {
      profile = this.device.getDeviceProfileManager().findProfile( aName );

      if ( profile != null )
      {
        LOG.log( Level.INFO, "Using device profile: {0}", profile.getDescription() );
      }
      else
      {
        LOG.log( Level.SEVERE, "No device profile found matching: {0}", aName );
      }
    }
    else
    {
      LOG.log( Level.SEVERE, "No device name provided by metadata! Cannot determine device profile..." );
    }

    return profile;
  }

  /**
   * @return the found device ID, or -1 if no suitable device ID was found.
   * @throws IOException
   */
  private int readDeviceId( final DataInputStream aInputStream ) throws IOException
  {
    int id = aInputStream.readInt();

    if ( id == SLA_V0 )
    {
      LOG.log( Level.INFO, "Found (unsupported!) Sump Logic Analyzer ...", Integer.toHexString( id ) );
    }
    else if ( id == SLA_V1 )
    {
      LOG.log( Level.INFO, "Found Sump Logic Analyzer/LogicSniffer compatible device ...", Integer.toHexString( id ) );
    }
    else
    {
      LOG.log( Level.INFO, "Found unknown device: 0x{0} ...", Integer.toHexString( id ) );
      id = -1;
    }
    return id;
  }

  /**
   * @param aMetadata
   * @throws IOException
   */
  private boolean readMetadata( final DataInputStream aInputStream, final LogicSnifferMetadata aMetadata )
      throws IOException
  {
    boolean gotResponse = false;
    int result = -1;

    do
    {
      try
      {
        result = aInputStream.read();

        if ( result > 0 )
        {
          // We've got response!
          gotResponse = true;

          final int type = ( result & 0xE0 ) >> 5;
          if ( type == 0x00 )
          {
            // key value is a null-terminated string...
            final String value = readString( aInputStream );
            aMetadata.put( result, value );
          }
          else if ( type == 0x01 )
          {
            // key value is a 32-bit integer; least significant byte first...
            // final Integer value = NumberUtils.convertByteOrder(
            // this.inputStream.readInt(), 32, ByteOrder.LITTLE_ENDIAN );
            final int value = aInputStream.readInt();
            aMetadata.put( result, Integer.valueOf( value ) );
          }
          else if ( type == 0x02 )
          {
            // key value is a 8-bit integer...
            final int value = aInputStream.read();
            aMetadata.put( result, Integer.valueOf( value ) );
          }
          else
          {
            LOG.log( Level.INFO, "Ignoring unknown metadata type: {0}", Integer.valueOf( type ) );
          }
        }
      }
      catch ( final IOException exception )
      {
        /* don't care */
        result = -1;

        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          throw exception;
        }
      }
    }
    while ( ( result > 0x00 ) && !Thread.currentThread().isInterrupted() );

    return gotResponse;
  }

  /**
   * Reads a zero-terminated ASCII-string from the current input stream.
   * 
   * @return the read string, can be empty but never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the string read.
   */
  private String readString( final InputStream aInputStream ) throws IOException
  {
    StringBuilder sb = new StringBuilder();

    int read = -1;
    do
    {
      read = aInputStream.read();
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

  /**
   * @param aOutputStream
   * @throws IOException
   */
  private void writeCmdGetDeviceId( final DataOutputStream aOutputStream ) throws IOException
  {
    aOutputStream.writeByte( CMD_ID );
    aOutputStream.flush();
  }

  /**
   * @throws IOException
   */
  private void writeCmdGetMetadata( final DataOutputStream aOutputStream ) throws IOException
  {
    aOutputStream.writeByte( CMD_METADATA );
    aOutputStream.flush();
  }

  /**
   * Resets the OLS device by sending 5 consecutive 'reset' commands.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  private void writeCmdReset( final DataOutputStream aOutputStream ) throws IOException
  {
    final byte[] resetSequence = new byte[5];
    Arrays.fill( resetSequence, ( byte )CMD_RESET );
    aOutputStream.write( resetSequence );
    aOutputStream.flush();
  }
}
