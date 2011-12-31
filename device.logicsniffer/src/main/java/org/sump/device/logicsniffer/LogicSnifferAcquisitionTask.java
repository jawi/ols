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
package org.sump.device.logicsniffer;


import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.*;

import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.protocol.*;
import org.sump.device.logicsniffer.sampleprocessor.*;


/**
 * Provides an acquisition task that uses the SUMP protocol for talking with a
 * LogicSniffer device on a serial/USB port.
 */
public class LogicSnifferAcquisitionTask implements SumpProtocolConstants, AcquisitionTask
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( LogicSnifferAcquisitionTask.class.getName() );

  // VARIABLES

  private final DeviceProfileManager deviceProfileManager;
  private final AcquisitionProgressListener acquisitionProgressListener;
  private final LogicSnifferConfig config;

  private StreamConnection connection;
  private SumpResultReader inputStream;
  private SumpCommandWriter outputStream;
  private int trigcount;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDevice instance.
   */
  public LogicSnifferAcquisitionTask( final LogicSnifferConfig aConfig, final StreamConnection aConnection,
      final DeviceProfileManager aDeviceProfileManager, final AcquisitionProgressListener aProgressListener )
  {
    this.config = aConfig;
    this.connection = aConnection;
    this.deviceProfileManager = aDeviceProfileManager;
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
  public AcquisitionResult call() throws IOException, InterruptedException
  {
    LOG.info( "Starting capture ..." );

    // Opens the device...
    open();

    // First try to find the logic sniffer itself...
    detectDevice();

    // check if data needs to be multiplexed
    final int channelCount = this.config.getChannelCount();
    if ( channelCount <= 0 )
    {
      throw new InternalError( "Internal error: did not obtain correct number of channels (" + channelCount + ")?!" );
    }

    final int sampleCount = this.config.getSampleCount();
    if ( sampleCount <= 0 )
    {
      throw new InternalError( "Internal error: did not obtain correct number of samples (" + sampleCount + ")?!" );
    }

    // Setup/configure the device with the UI-settings...
    configureAndArmDevice();

    final int[] buffer = new int[sampleCount];
    int sampleIdx = awaitTrigger( buffer );

    if ( LOG.isLoggable( Level.FINE ) && ( sampleIdx < sampleCount ) )
    {
      LOG.log( Level.FINE, "Trigger(s) fired! Reading {0} samples of {1} bytes ...",
          new Object[] { Integer.valueOf( sampleCount ), Integer.valueOf( this.config.getEnabledGroupCount() ) } );
    }

    // read all other samples
    sampleIdx = readSamples( sampleIdx, buffer );

    LOG.log( Level.FINE, "{0} samples read. Starting post processing...", Integer.valueOf( sampleCount - sampleIdx - 1 ) );

    final List<Integer> values = new ArrayList<Integer>();
    final List<Long> timestamps = new ArrayList<Long>();

    // collect additional information for CapturedData; we use arrays here,
    // as their values are to be filled from anonymous inner classes...
    final long[] absoluteLength = { 0L };
    final long[] triggerPos = { Ols.NOT_AVAILABLE };
    final int rate = this.config.getSampleRate();

    final SampleProcessorCallback callback = new SampleProcessorCallback()
    {
      public void addValue( final int aSampleValue, final long aTimestamp )
      {
        values.add( Integer.valueOf( aSampleValue ) );
        timestamps.add( Long.valueOf( aTimestamp ) );
      }

      public void ready( final long aAbsoluteLength, final long aTriggerPosition )
      {
        absoluteLength[0] = aAbsoluteLength;
        if ( LogicSnifferAcquisitionTask.this.config.isTriggerEnabled() )
        {
          triggerPos[0] = aTriggerPosition;
        }
      }
    };
    // Process the actual samples...
    createSampleProcessor( sampleCount, buffer, callback ).process();

    // Close the connection...
    close();

    return new CapturedData( values, timestamps, triggerPos[0], rate, channelCount,
        this.config.getEnabledChannelsMask(), absoluteLength[0] );
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
    this.trigcount = this.outputStream.writeDeviceConfiguration();

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
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          LOG.log( Level.WARNING, "Detaching failed!", exception );
        }
      }
      finally
      {
        HostUtils.closeResource( this.outputStream );
        HostUtils.closeResource( this.inputStream );

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
  protected final LogicSnifferConfig getConfig()
  {
    return this.config;
  }

  /**
   * Finds the device profile manager.
   * 
   * @return a device profile manager instance, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the device profile manager could not be found/obtained.
   */
  protected DeviceProfileManager getDeviceProfileManager()
  {
    return this.deviceProfileManager;
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
    final String portName = this.config.getPortName();
    final int openDelay = this.config.getOpenPortDelay();

    try
    {
      if ( conn == null )
      {
        throw new IOException( "Failed to open a valid connection!" );
      }

      this.outputStream = new SumpCommandWriter( this.config, conn.openDataOutputStream() );
      this.inputStream = new SumpResultReader( this.config, conn.openDataInputStream() );

      // Some devices need some time to initialize after being opened for the
      // first time, see issue #34.
      if ( openDelay > 0 )
      {
        Thread.sleep( openDelay );
      }

      // We don't expect any data, so flush all data pending in the given
      // input stream. See issue #34.
      this.inputStream.flush();
    }
    catch ( final Exception exception )
    {
      LOG.log( Level.WARNING, "Failed to open/use {0}! Possible reason: {1}",
          new Object[] { portName, exception.getMessage() } );
      LOG.log( Level.FINE, "Detailed stack trace:", exception );

      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        throw new IOException( "Failed to open/use " + portName + "! Possible reason: " + exception.getMessage() );
      }
    }
  }

  /**
   * Waits until the trigger is fired, or when the first sample is available (in
   * case no triggers are defined).
   * 
   * @param aBuffer
   *          the buffer to fill with the read sample.
   * @return the sample index after the first read/trigger.
   * @throws IOException
   *           in case of I/O problems;
   * @throws InterruptedException
   *           in case the acquisition is interrupted.
   */
  private int awaitTrigger( final int[] aBuffer ) throws IOException, InterruptedException
  {
    int sampleIdx = aBuffer.length - 1;
    boolean waiting = ( sampleIdx >= 0 );

    LOG.log( Level.FINE, "Awaiting trigger ..." );

    // wait for first byte forever (trigger could cause long initial delays)
    while ( waiting && !Thread.currentThread().isInterrupted() )
    {
      try
      {
        aBuffer[sampleIdx] = this.inputStream.readSample();
        sampleIdx--;
        waiting = false;
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          throw exception;
        }
      }
    }

    return sampleIdx;
  }

  /**
   * @param aSampleCount
   *          the actual number of samples to process;
   * @param aSampleValues
   *          the sample values to process;
   * @param aCallback
   *          the processor callback to use.
   * @return a sample processor instance, never <code>null</code>.
   */
  private SampleProcessor createSampleProcessor( final int aSampleCount, final int[] aSampleValues,
      final SampleProcessorCallback aCallback )
  {
    final SampleProcessor processor;
    if ( this.config.isRleEnabled() )
    {
      LOG.log( Level.INFO, "Decoding Run Length Encoded data, sample count: {0}", Integer.valueOf( aSampleCount ) );
      processor = new RleDecoder( this.config, aSampleValues, this.trigcount, aCallback );
    }
    else
    {
      LOG.log( Level.INFO, "Decoding unencoded data, sample count: {0}", Integer.valueOf( aSampleCount ) );
      processor = new EqualityFilter( this.config, aSampleValues, this.trigcount, aCallback );
    }
    return processor;
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
    int tries = 15;
    int id = -1;
    while ( ( tries-- >= 0 ) && ( id != SLA_V0 ) && ( id != SLA_V1 ) )
    {
      // make sure we're not blocking longer than strictly necessary...
      if ( Thread.currentThread().isInterrupted() )
      {
        return;
      }

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
      catch ( final EOFException exception )
      {
        // We're not able to finish our read; no further effort in detecting the
        // device is to be taken...
        id = -1;
        tries = -1;
      }
      catch ( final IOException exception )
      {
        /* don't care */
        id = -1;

        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          LOG.log( Level.INFO, "I/O exception!", exception );
        }
      }
    }

    if ( id == SLA_V0 )
    { // SLA0
      throw new IOException( "Device is obsolete. Please upgrade Firmware." );
    }
    else if ( id != SLA_V1 )
    { // SLA1
      throw new IOException( "Device not found!" );
    }

    // Try to find the metadata of the device, if returned, we can use it to
    // determine the capacities of the device...
    final LogicSnifferMetadata metadata = getDeviceMetadata();
    if ( metadata != null )
    {
      final DeviceProfile profile = getDeviceProfile( metadata );
      this.config.setDeviceProfile( profile );
    }
  }

  /**
   * Tries to obtain the OLS device's metadata.
   * 
   * @return the device metadata, can be not populated, but never
   *         <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  private LogicSnifferMetadata getDeviceMetadata() throws IOException
  {
    // Make sure nothing is left in our input buffer...
    this.inputStream.flush();

    // Ok; device appears to be good and willing to communicate; let's get its
    // metadata...
    this.outputStream.writeCmdGetMetadata();

    boolean gotResponse = false;

    try
    {
      final LogicSnifferMetadata metadata = new LogicSnifferMetadata();

      gotResponse = this.inputStream.readMetadata( metadata );

      return metadata;
    }
    finally
    {
      if ( !gotResponse )
      {
        // Reset the device again; this ensures correct working for devices
        // whose firmware do not understand the metadata command...
        this.outputStream.writeCmdReset();
      }
    }
  }

  /**
   * Determines the device profile for the current attached device. The device
   * profile provides us with detailed information about the capabilities of a
   * certain SUMP-compatible device.
   * 
   * @param aMetadata
   *          the device metadata, can be <code>null</code>.
   * @return a device profile, or <code>null</code> if no such profile could be
   *         determined.
   */
  private DeviceProfile getDeviceProfile( final LogicSnifferMetadata aMetadata )
  {
    DeviceProfile profile = null;
    if ( aMetadata != null )
    {
      // Log the read results...
      LOG.log( Level.INFO, "Detected device type: {0}", aMetadata.getName() );
      LOG.log( Level.FINE, "Device metadata = \n{0}", aMetadata.toString() );

      final String name = aMetadata.getName();
      if ( name != null )
      {
        final DeviceProfileManager manager = getDeviceProfileManager();
        profile = manager.findProfile( name );

        if ( profile != null )
        {
          LOG.log( Level.INFO, "Using device profile: {0}", profile.getDescription() );
        }
        else
        {
          LOG.log( Level.SEVERE, "No device profile found matching: {0}", name );
        }
      }
    }
    return profile;
  }

  /**
   * Reads the remaining samples.
   * 
   * @param aSampleIdx
   *          the sample index where to start filling samples in the given
   *          buffer;
   * @param aBuffer
   *          the buffer to fill with sample data.
   * @return the final sample index, >= 0 && < aBuffer.length.
   * @throws IOException
   *           in case of I/O problems;
   * @throws InterruptedException
   *           in case the current thread was interrupted.
   */
  private int readSamples( int aSampleIdx, final int[] aBuffer ) throws IOException, InterruptedException
  {
    try
    {
      for ( ; ( aSampleIdx >= 0 ) && !Thread.currentThread().isInterrupted(); aSampleIdx-- )
      {
        aBuffer[aSampleIdx] = this.inputStream.readSample();

        final int percentage = ( int )( 100.0 - ( ( 100.0 * aSampleIdx ) / aBuffer.length ) );
        this.acquisitionProgressListener.acquisitionInProgress( percentage );
      }
    }
    catch ( IOException exception )
    {
      // Make sure we leave the device in a correct state...
      this.outputStream.writeCmdReset();

      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        throw exception;
      }
    }
    finally
    {
      this.acquisitionProgressListener.acquisitionInProgress( 100 );
    }

    // In case the device sends its samples in "reverse" order, we need to
    // revert it now, before processing them further...
    if ( this.config.isSamplesInReverseOrder() )
    {
      HostUtils.reverse( aBuffer );
    }

    return aSampleIdx;
  }
}
