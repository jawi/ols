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
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;
import org.sump.device.logicsniffer.sampleprocessor.*;


/**
 * Provides an acquisition task that uses the SUMP protocol for talking with a
 * LogicSniffer device on a serial/USB port.
 */
public class LogicSnifferAcquisitionTask implements AcquisitionTask
{
  // CONSTANTS

  public static final String PROP_CAPTURE_PROGRESS = "progress";
  public static final String PROP_CAPTURE_STATE = "state";

  /** The default sample clock in Hertz (Hz). */
  public static final int CLOCK = 100000000; // device clock in Hz

  /** Old SLA version, v0 (0x534c4130, or 0x30414c53) - no longer supported. */
  private static final int SLA_V0 = 0x30414c53;
  /** Current SLA version, v1 (0x534c4131, or 0x31414c53) - supported. */
  private static final int SLA_V1 = 0x31414c53;

  /** set trigger mask */
  private static final int SETTRIGMASK = 0xc0;
  /** set trigger value */
  private static final int SETTRIGVAL = 0xc1;
  /** set trigger configuration */
  private static final int SETTRIGCFG = 0xc2;
  /** set clock divider */
  private static final int SETDIVIDER = 0x80;
  /** set sample counters */
  private static final int SETSIZE = 0x81;
  /** set flags */
  private static final int SETFLAGS = 0x82;

  /** reset analyzer */
  private static final int CMD_RESET = 0x00;
  /** arm trigger / run device */
  private static final int CMD_RUN = 0x01;
  /** ask for device id */
  private static final int CMD_ID = 0x02;
  /** ask for device self test. */
  private static final int CMD_SELFTEST = 0x03;
  /** ask for device meta data. */
  private static final int CMD_METADATA = 0x04;

  // demultiplex
  static final int FLAG_DEMUX = 0x00000001;
  // noise filter
  static final int FLAG_FILTER = 0x00000002;
  // channel group 1 enabled?
  static final int FLAG_GROUP1_DISABLED = 0x00000004;
  // channel group 2 enabled?
  static final int FLAG_GROUP2_DISABLED = 0x00000008;
  // channel group 3 enabled?
  static final int FLAG_GROUP3_DISABLED = 0x00000010;
  // channel group 4 enabled?
  static final int FLAG_GROUP4_DISABLED = 0x00000020;
  // external trigger?
  static final int FLAG_EXTERNAL = 0x00000040;
  // inverted
  static final int FLAG_INVERTED = 0x00000080;
  // run length encoding
  static final int FLAG_RLE = 0x00000100;
  static final int FLAG_RLE_MODE_0 = 0x00000000;
  static final int FLAG_RLE_MODE_1 = 0x00004000;
  static final int FLAG_RLE_MODE_2 = 0x00008000;
  static final int FLAG_RLE_MODE_3 = 0x0000C000;
  // Number Scheme
  static final int FLAG_NUMBER_SCHEME = 0x00000200;
  // Testing mode (external, bits 16:32)
  static final int FLAG_EXTERNAL_TEST_MODE = 0x00000400;
  // Testing mode (internal)
  static final int FLAG_INTERNAL_TEST_MODE = 0x00000800;

  private static final Logger LOG = Logger.getLogger( LogicSnifferAcquisitionTask.class.getName() );

  // VARIABLES

  private final DeviceProfileManager deviceProfileManager;
  private final AcquisitionProgressListener acquisitionProgressListener;
  private final LogicSnifferConfig config;

  private StreamConnection connection;
  private DataInputStream inputStream;
  private DataOutputStream outputStream;
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
   * Determines which RLE-mode to use. There are up to four different RLE-modes
   * present in 'dogsbody' Verilog firmware.
   * <p>
   * The RLE-Encoding modes are:
   * </p>
   * <ol start="0">
   * <li>Issue {values} & {RLE-count} as pairs. Count inclusive of value
   * (<strike>backwards compatible</strike>);</li>
   * <li>Issue {values} & {RLE-count} as pairs. Count is <em>exclusive</em> of
   * value. Compatible with all clients;</li>
   * <li>Periodic. {values} reissued approximately every 256 {RLE-count} fields;
   * </li>
   * <li>Unlimited. {values} can be followed by unlimited numbers of
   * {RLE-counts}.</li>
   * </ol>
   * 
   * @return a RLE-mode, defaults to 1.
   */
  private static int determineRleMode()
  {
    return NumberUtils.smartParseInt( System.getProperty( "nl.lxtreme.ols.rle.mode" ), 1 );
  }

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

    final int samples = this.config.getSampleCount();
    if ( samples <= 0 )
    {
      throw new InternalError( "Internal error: did not obtain correct number of samples (" + samples + ")?!" );
    }

    // Setup/configure the device with the UI-settings...
    configureDevice();

    // We're ready to process the samples from the device...
    sendCommand( CMD_RUN );

    final int[] buffer = new int[samples];
    int sampleIdx = samples - 1;
    boolean waiting = ( sampleIdx >= 0 );

    LOG.log( Level.FINE, "Awaiting trigger ..." );

    // wait for first byte forever (trigger could cause long initial delays)
    while ( waiting && !Thread.currentThread().isInterrupted() )
    {
      try
      {
        buffer[sampleIdx] = readSample();
        sampleIdx--;
        waiting = false;
      }
      catch ( final InterruptedException exception )
      {
        // When running, we simply have a timeout; this could be that the
        // trigger is not fired yet... We keep waiting...
        if ( Thread.currentThread().isInterrupted() )
        {
          // Make sure to handle IO-interrupted exceptions properly!
          if ( !HostUtils.handleInterruptedException( exception ) )
          {
            throw exception;
          }
        }
      }
    }

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.log( Level.FINE, "Trigger(s) fired! Reading {0} samples of {1} bytes ...",
          new Object[] { Integer.valueOf( samples ), Integer.valueOf( this.config.getEnabledGroupCount() ) } );
    }

    // read all other samples
    try
    {
      for ( ; ( sampleIdx >= 0 ) && !Thread.currentThread().isInterrupted(); sampleIdx-- )
      {
        buffer[sampleIdx] = readSample();
        this.acquisitionProgressListener
            .acquisitionInProgress( ( int )( 100.0 - ( ( 100.0 * sampleIdx ) / buffer.length ) ) );
      }
    }
    catch ( InterruptedException exception )
    {
      LOG.log( Level.WARNING, "Capture interrupted! Only {0} samples read ...", Integer.valueOf( samples - sampleIdx ) );

      if ( Thread.currentThread().isInterrupted() )
      {
        // Make sure the device is in a state were we can do something with
        // it after this method is completed...
        resetDevice();
      }

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

    // Close the connection...
    close();

    LOG.log( Level.FINE, "{0} samples read. Starting post processing...", Integer.valueOf( samples - sampleIdx - 1 ) );

    // In case the device sends its samples in "reverse" order, we need to
    // revert it now, before processing them further...
    if ( this.config.isSamplesInReverseOrder() )
    {
      HostUtils.reverse( buffer );
    }

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
    createSampleProcessor( samples, buffer, callback ).process();

    return new CapturedData( values, timestamps, triggerPos[0], rate, channelCount,
        this.config.getEnabledChannelsMask(), absoluteLength[0] );
  }

  /**
   * Performs a self-test on the OLS device.
   * <p>
   * Note: not all versions of the OLS device firmware support a selftest!
   * </p>
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  public void selfTest() throws IOException
  {
    sendCommand( CMD_SELFTEST );
    // TODO read selftest result...
  }

  /**
   * Configures the OLS device by sending all configuration commands.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  final void configureDevice() throws IOException
  {
    // set the sampling frequency...
    sendCommand( SETDIVIDER, this.config.getDivider() );

    final int stopCounter = configureTriggers();
    final int readCounter = this.config.getReadCounter();

    final int size;
    if ( this.config.isDoubleDataRateEnabled() )
    {
      // 0x7fff8 = 511Kb = the maximum size supported by the original SUMP
      // device when using the demultiplexer...
      final int maxSize = 0x7fff8;
      size = ( ( stopCounter & maxSize ) << 13 ) | ( ( ( readCounter & maxSize ) >> 3 ) - 1 );
      // A better approximation of "(readCounter - stopCounter) - 2". This also
      // solves issue #31...
      this.trigcount = ( ( ( size & 0xffff ) << 3 ) - ( ( ( size >> 16 ) & 0xffff ) << 3 ) );
    }
    else
    {
      // 0x3fffc = 255Kb = the maximum size supported by the original SUMP
      // device...
      final int maxSize = 0x3fffc;
      size = ( ( stopCounter & maxSize ) << 14 ) | ( ( ( readCounter & maxSize ) >> 2 ) - 1 );
      // A better approximation of "(readCounter - stopCounter) - 2". This also
      // solves issue #31...
      this.trigcount = ( ( ( size & 0xffff ) << 2 ) - ( ( ( size >> 16 ) & 0xffff ) << 2 ) );
    }

    // set the capture size...
    sendCommand( SETSIZE, size );

    int flags = 0;
    if ( this.config.isExternalClock() )
    {
      flags |= FLAG_EXTERNAL;
      if ( CaptureClockSource.EXTERNAL_FALLING == this.config.getClockSource() )
      {
        flags |= FLAG_INVERTED;
      }
    }

    // determine which channel groups are to be disabled...
    int enabledChannelGroups = 0;
    for ( int i = 0; i < this.config.getGroupCount(); i++ )
    {
      if ( this.config.isGroupEnabled( i ) )
      {
        enabledChannelGroups |= ( 1 << i );
      }
    }

    if ( this.config.isDoubleDataRateEnabled() )
    {
      // when DDR is selected, the groups selected in the upper two channel
      // groups must be the same as those selected in the lower two groups
      enabledChannelGroups |= ( ( enabledChannelGroups & 0x03 ) << 2 ) & 0x0c;

      flags |= FLAG_DEMUX;
      // if the demux bit is set, the filter flag *must* be cleared...
      flags &= ~FLAG_FILTER;
    }

    flags |= ~( enabledChannelGroups << 2 ) & 0x3c;

    if ( this.config.isFilterEnabled() && this.config.isFilterAvailable() )
    {
      flags |= FLAG_FILTER;
      // if the filter bit is set, the demux flag *must* be cleared...
      flags &= ~FLAG_DEMUX;
    }

    if ( this.config.isRleEnabled() )
    {
      flags |= FLAG_RLE;

      // Ian 'dogsbody''s Verilog understands four different RLE-modes...
      final int rleMode = determineRleMode();
      switch ( rleMode )
      {
        case 3:
          flags |= FLAG_RLE_MODE_3;
          break;
        case 2:
          flags |= FLAG_RLE_MODE_2;
          break;
        case 0:
          flags |= FLAG_RLE_MODE_0;
          break;
        default:
          flags |= FLAG_RLE_MODE_1;
          break;
      }
    }

    if ( this.config.isAltNumberSchemeEnabled() )
    {
      flags |= FLAG_NUMBER_SCHEME;
    }

    if ( this.config.isTestModeEnabled() )
    {
      flags |= FLAG_EXTERNAL_TEST_MODE;
    }

    LOG.log( Level.FINE, "Flags: 0b{0}", Integer.toBinaryString( flags ) );

    // finally set the device flags...
    sendCommand( SETFLAGS, flags );
  }

  /**
   * Sends the trigger mask, value and configuration to the OLS device.
   * 
   * @return the stop counter that is used for the trigger configuration.
   * @throws IOException
   *           in case of I/O problems.
   */
  final int configureTriggers() throws IOException
  {
    final int effectiveStopCounter;
    if ( this.config.isTriggerEnabled() )
    {
      for ( int i = 0; i < this.config.getMaxTriggerStages(); i++ )
      {
        final int indexMask = 4 * i;
        sendCommand( SETTRIGMASK | indexMask, this.config.getTriggerMask( i ) );
        sendCommand( SETTRIGVAL | indexMask, this.config.getTriggerValue( i ) );
        sendCommand( SETTRIGCFG | indexMask, this.config.getTriggerConfig( i ) );
      }
      effectiveStopCounter = this.config.getStopCounter();
    }
    else
    {
      sendCommand( SETTRIGMASK, 0 );
      sendCommand( SETTRIGVAL, 0 );
      sendCommand( SETTRIGCFG, LogicSnifferConfig.TRIGGER_CAPTURE );
      effectiveStopCounter = this.config.getReadCounter();
    }

    return effectiveStopCounter;
  }

  /**
   * Reads a single sample (= 1..4 bytes) from the serial input stream.
   * <p>
   * This method will take the enabled channel groups into consideration, making
   * it possible that the returned value contains "gaps".
   * </p>
   * 
   * @return the integer sample value containing up to four read bytes, not
   *         aligned.
   * @throws IOException
   *           if stream reading fails.
   */
  final int readSample() throws IOException, InterruptedException
  {
    final int groupCount = this.config.getGroupCount();
    byte[] buf = new byte[groupCount];

    final int enabledGroupCount = this.config.getEnabledGroupCount();
    assert enabledGroupCount > 0 : "Internal error: enabled group count should be at least 1!";
    assert enabledGroupCount <= groupCount : "Internal error: enabled group count be at most " + groupCount;

    int read, offset = 0;
    do
    {
      // Issue #81: read the same amount of bytes as given in the enabled group
      // count; otherwise succeeding reads might fail and/or data offset errors
      // could occur...
      read = this.inputStream.read( buf, offset, enabledGroupCount - offset );
      if ( read < 0 )
      {
        throw new EOFException( "Data readout interrupted: EOF." );
      }
      if ( Thread.currentThread().isInterrupted() )
      {
        throw new InterruptedException( "Data readout interrupted." );
      }
      offset += read;
    }
    while ( offset < enabledGroupCount );

    // "Expand" the read sample-bytes into a single sample value...
    int value = 0;

    for ( int i = 0, j = 0; i < groupCount; i++ )
    {
      // in case the group is disabled, simply set it to zero...
      if ( this.config.isGroupEnabled( i ) )
      {
        value |= ( ( buf[j++] & 0xff ) << ( 8 * i ) );
      }
    }

    return value;
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

  /**
   * Sends a short command to the given stream. This method is intended to be
   * used for short commands, but can also be called with long command opcodes
   * if the data portion is to be set to 0.
   * 
   * @param aOpcode
   *          one byte operation code
   * @throws IOException
   *           if writing to stream fails
   */
  final void sendCommand( final int aOpcode ) throws IOException
  {
    if ( LOG.isLoggable( Level.ALL ) || LOG.isLoggable( Level.FINE ) )
    {
      final byte opcode = ( byte )( aOpcode & 0xFF );
      LOG.log( Level.FINE, "Sending short command: {0} ({1})",
          new Object[] { Integer.toHexString( opcode ), Integer.toBinaryString( opcode ) } );
    }

    this.outputStream.writeByte( aOpcode );
    this.outputStream.flush();
  }

  /**
   * Sends a long command to the given stream.
   * 
   * @param aOpcode
   *          one byte operation code
   * @param aData
   *          four byte data portion
   * @throws IOException
   *           if writing to stream fails
   */
  final void sendCommand( final int aOpcode, final int aData ) throws IOException
  {
    if ( LOG.isLoggable( Level.ALL ) || LOG.isLoggable( Level.FINE ) )
    {
      final byte opcode = ( byte )( aOpcode & 0xFF );
      LOG.log( Level.FINE, "Sending long command: {0} ({1}) with data {2} ({3})",
          new Object[] { Integer.toHexString( opcode ), Integer.toBinaryString( opcode ), //
              Integer.toHexString( aData ), Integer.toBinaryString( aData ) } );
    }

    final byte[] raw = new byte[5];
    int mask = 0xff;
    int shift = 0;

    raw[0] = ( byte )aOpcode;
    for ( int i = 1; i < 5; i++ )
    {
      raw[i] = ( byte )( ( aData & mask ) >> shift );
      mask = mask << 8;
      shift += 8;
    }

    this.outputStream.write( raw );
    this.outputStream.flush();
  }

  /**
   * Detaches the currently attached port, if one exists. This will close the
   * serial port.
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
          sendCommand( CMD_RESET );
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

      this.outputStream = conn.openDataOutputStream();
      this.inputStream = conn.openDataInputStream();

      // Some devices need some time to initialize after being opened for the
      // first time, see issue #34.
      if ( openDelay > 0 )
      {
        Thread.sleep( openDelay );
      }

      // We don't expect any data, so flush all data pending in the given
      // input stream. See issue #34.
      HostUtils.flushInputStream( this.inputStream );
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
   * @param aSamples
   * @param aBuffer
   * @param aCallback
   * @return
   */
  private SampleProcessor createSampleProcessor( final int aSamples, final int[] aBuffer,
      final SampleProcessorCallback aCallback )
  {
    final SampleProcessor processor;
    if ( this.config.isRleEnabled() )
    {
      LOG.log( Level.INFO, "Decoding Run Length Encoded data, sample count: {0}", Integer.valueOf( aSamples ) );
      processor = new RleDecoder( this.config, aBuffer, this.trigcount, aCallback );
    }
    else
    {
      LOG.log( Level.INFO, "Decoding unencoded data, sample count: {0}", Integer.valueOf( aSamples ) );
      processor = new EqualityFilter( this.config, aBuffer, this.trigcount, aCallback );
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
      HostUtils.flushInputStream( this.inputStream );

      resetDevice();

      // check if device is ready
      sendCommand( CMD_ID );

      try
      {
        id = this.inputStream.readInt();

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
        }
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

    if ( this.config.getDeviceProfile() != null )
    {
      // Try to find the metadata of the device, if returned, we can use it to
      // determine the capacities of the device...
      final LogicSnifferMetadata metadata = getMetadata();
      if ( metadata != null )
      {
        // Log the read results...
        LOG.log( Level.INFO, "Detected device type: {0}", metadata.getName() );
        LOG.log( Level.FINE, "Device metadata = \n{0}", metadata.toString() );

        final String name = metadata.getName();
        if ( name != null )
        {
          final DeviceProfileManager manager = getDeviceProfileManager();
          final DeviceProfile profile = manager.findProfile( name );

          if ( profile != null )
          {
            LOG.log( Level.INFO, "Using device profile: {0}", profile.getDescription() );
          }
          else
          {
            LOG.log( Level.SEVERE, "No device profile found matching: {0}", name );
          }

          this.config.setDeviceProfile( profile );
        }
      }
    }
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
  private LogicSnifferMetadata getMetadata() throws IOException, IllegalStateException
  {
    // Make sure nothing is left in our input buffer...
    HostUtils.flushInputStream( this.inputStream );

    // Ok; device appears to be good and willing to communicate; let's get its
    // metadata...
    sendCommand( CMD_METADATA );

    boolean gotResponse = false;

    try
    {
      final LogicSnifferMetadata metadata = new LogicSnifferMetadata();

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
              metadata.put( result, value );
            }
            else if ( type == 0x01 )
            {
              // key value is a 32-bit integer; least significant byte first...
              // final Integer value = NumberUtils.convertByteOrder(
              // this.inputStream.readInt(), 32, ByteOrder.LITTLE_ENDIAN );
              final int value = this.inputStream.readInt();
              metadata.put( result, Integer.valueOf( value ) );
            }
            else if ( type == 0x02 )
            {
              // key value is a 8-bit integer...
              final int value = this.inputStream.read();
              metadata.put( result, Integer.valueOf( value ) );
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
            LOG.log( Level.INFO, "I/O exception", exception );
          }
        }
      }
      while ( ( result > 0x00 ) && !Thread.currentThread().isInterrupted() );

      return metadata;
    }
    finally
    {
      if ( !gotResponse )
      {
        // Reset the device again; this ensures correct working for devices
        // whose firmware do not understand the metadata command...
        resetDevice();
      }
    }
  }

  /**
   * Resets the OLS device by sending 5 consecutive 'reset' commands.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  private void resetDevice() throws IOException
  {
    for ( int i = 0; i < 5; i++ )
    {
      sendCommand( CMD_RESET );
    }
  }
}
