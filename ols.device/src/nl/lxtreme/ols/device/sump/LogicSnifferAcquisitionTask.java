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
package nl.lxtreme.ols.device.sump;


import java.io.*;
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.sump.protocol.*;
import nl.lxtreme.ols.task.execution.*;


/**
 * Provides an acquisition task that uses the SUMP protocol for talking with a
 * LogicSniffer device on a serial/USB port.
 */
public class LogicSnifferAcquisitionTask implements SumpProtocolConstants, Task<AcquisitionData>
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( LogicSnifferAcquisitionTask.class.getName() );

  // VARIABLES

  private final AcquisitionProgressListener acquisitionProgressListener;
  private final SumpConfig config;

  private StreamConnection connection;
  private SumpResultReader inputStream;
  private SumpCommandWriter outputStream;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDevice instance.
   */
  public LogicSnifferAcquisitionTask( SumpConfig aConfig, StreamConnection aConnection,
      AcquisitionProgressListener aProgressListener )
  {
    this.config = aConfig;
    this.connection = aConnection;
    this.acquisitionProgressListener = aProgressListener;
  }

  // METHODS

  /**
   * Sends the configuration to the device, starts it, reads the captured data
   * and returns a CapturedData object containing the data read as well as
   * device configuration information.
   * 
   * @return the captured results, never <code>null</code>.
   * @throws IOException
   *           when writing to or reading from device fails
   * @throws InterruptedException
   *           if a read time out occurs after trigger match or stop() was
   *           called before trigger match
   */
  @Override
  public AcquisitionData call() throws IOException, InterruptedException
  {
    try
    {
      // Opens the device...
      open();

      // First try to find the logic sniffer itself...
      detectDevice();

      final int sampleCount = this.config.getSampleCount();
      if ( sampleCount <= 0 )
      {
        throw new InternalError( "Internal error: did not obtain correct number of samples (" + sampleCount + ")?!" );
      }

      // Setup/configure the device with the UI-settings...
      configureAndArmDevice();

      // read all samples
      return readSampleData( this.config.getEnabledGroupCount(), sampleCount );
    }
    finally
    {
      // Close the connection...
      close();
    }
  }

  /**
   * Configures the device and arms it so that it starts capturing. Performs a
   * self-test on the OLS device.
   * <p>
   * Note: not all versions of the OLS device firmware support a selftest!
   * </p>
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  void configureAndArmDevice() throws IOException
  {
    LOG.info( "Configuring device ..." );

    this.outputStream.writeDeviceConfiguration();

    LOG.info( "Arming device and starting capture ..." );

    // We're ready to process the samples from the device...
    this.outputStream.writeCmdRun();
  }

  /**
   * Factory method to create a sample procesor for the given numer of samples
   * and sample values.
   * 
   * @param aSampleCount
   *          the sample count;
   * @param aSampleValues
   *          the sample values; Detaches the currently attached port, if one
   *          exists. This will close the serial port.
   */
  protected void close()
  {
    StreamConnection conn = getStreamConnection();
    if ( conn != null )
    {
      try
      {
        // Make sure to flush any pending information we did not read for some
        // reason...
        if ( this.inputStream != null )
        {
          this.inputStream.flush();
        }

        // try to make sure device is reset...
        if ( this.outputStream != null )
        {
          // XXX it seems that after a RLE abort command, the OLS device no
          // longer is able to process a full 5x reset command. However, we're
          // also resetting the thing right after we've started an acquisition,
          // so it might not be that bad...
          this.outputStream.writeCmdReset();
        }
      }
      catch ( final IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !handleInterruptedException( exception ) )
        {
          LOG.log( Level.WARNING, "Closing of device failed!", exception );
        }
      }
      finally
      {
        closeSilently( this.outputStream );
        closeSilently( this.inputStream );

        try
        {
          conn.close();
        }
        catch ( IOException exception )
        {
          LOG.log( Level.WARNING, "Closing connection failed!", exception );
        }
        finally
        {
          this.connection = null;
          this.outputStream = null;
          this.inputStream = null;
        }
      }
    }
  }

  /**
   * Returns the configuration as used for this device.
   * 
   * @return a device configuration, never <code>null</code>.
   */
  protected final SumpConfig getConfig()
  {
    return this.config;
  }

  /**
   * @return
   */
  protected StreamConnection getStreamConnection()
  {
    return this.connection;
  }

  /**
   * Opens the incoming and outgoing connection to the OLS device.
   * <p>
   * This method will directly flush all incoming data, and, if configured,
   * delay a bit to ensure the device hardware is properly initialized.
   * </p>
   * 
   * @return <code>true</code> if the attach operation succeeded,
   *         <code>false</code> otherwise.
   * @throws IOException
   *           in case of I/O problems during attaching to the device.
   */
  protected void open() throws IOException
  {
    final StreamConnection conn = getStreamConnection();

    try
    {
      LOG.fine( "Opening connection to device ..." );

      if ( conn == null )
      {
        throw new IOException( "Failed to open a valid connection!" );
      }

      this.outputStream = new SumpCommandWriter( this.config, conn.openDataOutputStream() );
      this.inputStream = new SumpResultReader( conn.openDataInputStream() );

      // We don't expect any data, so flush all data pending in the given
      // input stream. See issue #34.
      this.inputStream.flush();
    }
    catch ( final Exception exception )
    {
      LOG.log( Level.WARNING, "Failed to open connection! Possible reason: " + exception.getMessage() );
      LOG.log( Level.FINE, "Detailed stack trace:", exception );

      // Make sure to handle IO-interrupted exceptions properly!
      if ( !handleInterruptedException( exception ) )
      {
        throw new IOException( "Failed to open connection! Possible reason: " + exception.getMessage() );
      }
    }
  }

  private void closeSilently( Closeable aClosable )
  {
    try
    {
      if ( aClosable != null )
      {
        aClosable.close();
      }
    }
    catch ( IOException exception )
    {
      // Ignore...
    }
  }

  /**
   * Tries to detect the LogicSniffer device.
   * 
   * @return the device's metadata, never <code>null</code>.
   * @throws IOException
   *           in case the device could not be found, or in case of any other
   *           I/O problem.
   */
  private void detectDevice() throws IOException
  {
    LOG.fine( "Detecting device ..." );

    int tries = 3;
    int id = -1;
    do
    {
      // Make sure nothing is left in our input buffer...
      this.inputStream.flush();

      // reset the device first; to ensure it is in the proper initial state...
      this.outputStream.writeCmdReset();

      // check if device is ready
      this.outputStream.writeCmdGetId();

      try
      {
        id = this.inputStream.readDeviceId();
      }
      catch ( EOFException exception )
      {
        // We're not able to finish our read; no further effort in detecting the
        // device is to be taken...
        id = -1;
        tries = -1;
      }
      catch ( IOException exception )
      {
        /* don't care */
        id = -1;

        // Make sure to handle IO-interrupted exceptions properly!
        if ( !handleInterruptedException( exception ) )
        {
          LOG.log( Level.INFO, "I/O exception!", exception );
        }
      }
    }
    while ( !Thread.currentThread().isInterrupted() && ( tries-- > 0 ) && ( id < 0 ) );

    if ( id == SLA_V0 )
    { // SLA0
      LOG.fine( "Obsolete device 'SLA0' found ..." );

      throw new IOException( "Device is obsolete. Please upgrade firmware." );
    }
    else if ( id != SLA_V1 )
    { // SLA1
      LOG.fine( "Unknown device (" + Integer.toHexString( id ) + ") found ..." );

      throw new IOException( "Device not found!" );
    }

    LOG.fine( "Device 'SLA1' found ..." );
  }

  private boolean handleInterruptedException( Exception aException )
  {
    if ( aException instanceof InterruptedIOException )
    {
      Thread.currentThread().interrupt();
      return true;
    }
    return false;
  }

  /**
   * Reads all (or as many as possible) samples from the OLS device.
   * 
   * @param aEnabledGroupCount
   *          the number of enabled groups (denotes the number of bytes for one
   *          sample);
   * @param aSampleCount
   *          the number of samples to read.
   * @return the read samples, normalized to match the layout of the enabled
   *         groups.
   * @throws IOException
   *           in case of I/O problems;
   * @throws InterruptedException
   *           in case the current thread was interrupted.
   */
  private AcquisitionData readSampleData( final int aEnabledGroupCount, int aSampleCount ) throws IOException,
      InterruptedException
  {
    LOG.fine( "Awaiting data and processing sample information ..." );

    final int length = aEnabledGroupCount * aSampleCount;
    final byte[] rawData = new byte[length];

    try
    {
      int offset = 0;
      int zerosRead = 0;
      int count = length;
      while ( !Thread.currentThread().isInterrupted() && ( offset >= 0 ) && ( offset < length ) )
      {
        int read = this.inputStream.readRawData( rawData, offset, count );
        if ( read < 0 )
        {
          throw new EOFException();
        }
        else if ( read == 0 )
        {
          System.out.printf("Read zero bytes?! Stats = [%d/%d/%d]%n", offset, count, zerosRead);
          if ( ++zerosRead == 10000 )
          {
            throw new IOException( "Device did not respond with any data within valid time bound!" );
          }
        }
        else
        {
          zerosRead = 0;
          count -= read;
          offset += read;
        }

        this.acquisitionProgressListener.acquisitionInProgress( ( 100 * offset ) / length );
      }
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !handleInterruptedException( exception ) )
      {
        throw exception;
      }
    }
    finally
    {
      this.acquisitionProgressListener.acquisitionInProgress( 100 );
    }

    if ( Thread.currentThread().isInterrupted() )
    {
      // We're interrupted while read samples, do not proceed...
      throw new InterruptedException();
    }

    return new SumpAcquisitionDataBuilder( this.config ).build( rawData, this.acquisitionProgressListener );
  }
}
