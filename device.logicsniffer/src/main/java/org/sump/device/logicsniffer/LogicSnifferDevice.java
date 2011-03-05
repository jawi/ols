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
import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;

import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;


/**
 * Device provides access to the physical logic analyzer device. It requires the
 * rxtx package from {@link http://www.rxtx.org/} to access the serial port the
 * analyzer is connected to.
 */
public abstract class LogicSnifferDevice extends SwingWorker<CapturedData, Sample>
{
  // INNER TYPES

  /**
   * Processes all samples and only returns the actual changed sample values.
   */
  final class EqualityFilter implements SampleProcessor
  {
    // VARIABLES

    private final int[] buffer;
    private final int trigCount;
    private final SampleProcessorCallback callback;

    // CONSTRUCTORS

    /**
     * @param aBuffer
     *          the buffer with sample data to decode.
     * @param aTrigCount
     *          the trigcount value;
     * @param aCallback
     *          the callback to use.
     */
    public EqualityFilter( final int[] aBuffer, final int aTrigCount, final SampleProcessorCallback aCallback )
    {
      if ( aBuffer == null )
      {
        throw new IllegalArgumentException( "Buffer cannot be null!" );
      }
      this.buffer = aBuffer;
      this.trigCount = aTrigCount;
      this.callback = aCallback;
    }

    // METHODS

    /**
     * @see org.sump.device.logicsniffer.LogicSnifferDevice.SampleProcessor#process()
     */
    @Override
    public final void process()
    {
      long time = 0;

      final int samples = this.buffer.length;

      int oldSample = 0; // first value doesn't really matter
      for ( int i = 0; i < samples; i++ )
      {
        final int newSample = this.buffer[i];

        if ( ( i == 0 ) || ( oldSample != newSample ) )
        {
          // add the read sample & add a timestamp value as well...
          this.callback.addValue( newSample, time );
        }

        oldSample = newSample;
        time++;
      }

      // XXX JaWi: why is this correction needed?
      int correction = 2;
      if ( LogicSnifferDevice.this.config.getDivider() <= 3 )
      {
        correction = 1;
      }

      // Take the last seen time value as "absolete" length of this trace...
      this.callback.ready( time, ( this.trigCount - correction ) );
    }
  }

  /**
   * Provides a RLE decoder.
   */
  final class RleDecoder implements SampleProcessor
  {
    // VARIABLES

    private final int[] buffer;
    private final int trigCount;
    private final SampleProcessorCallback callback;

    private final int rleCountValue;
    private final int rleCountMask;

    // CONSTRUCTORS

    /**
     * @param aBuffer
     *          the buffer with sample data to decode.
     * @param aTrigCount
     *          the trigcount value;
     * @param aCallback
     *          the callback to use.
     */
    public RleDecoder( final int[] aBuffer, final int aTrigCount, final SampleProcessorCallback aCallback )
    {
      if ( aBuffer == null )
      {
        throw new IllegalArgumentException( "Buffer cannot be null!" );
      }
      this.buffer = aBuffer;
      this.trigCount = aTrigCount;
      this.callback = aCallback;

      // enabled group count is "automatically" corrected for DDR/Demux mode...
      final int width = LogicSnifferDevice.this.config.getRLEDataWidth();
      switch ( width )
      {
        case 32:
          this.rleCountValue = 0x80000000;
          this.rleCountMask = this.rleCountValue - 1;
          break;
        case 24:
          this.rleCountValue = 0x800000;
          this.rleCountMask = this.rleCountValue - 1;
          break;
        case 16:
          this.rleCountValue = 0x8000;
          this.rleCountMask = this.rleCountValue - 1;
          break;
        case 8:
          this.rleCountValue = 0x80;
          this.rleCountMask = this.rleCountValue - 1;
          break;
        default:
          throw new IllegalArgumentException( "Illegal RLE width! Should be 8, 16, 24 or 32!" );
      }
    }

    // METHODS

    /**
     * @see org.sump.device.logicsniffer.LogicSnifferDevice.SampleProcessor#process()
     */
    public void process()
    {
      long time = 0;
      long rleTrigPos = 0;
      int oldSample = -1;

      // if msb set increment time by the count value
      // else save sample check trigger pos and increment time by 1
      // this should work for either dogsbody or rasmus bitstreams

      final int samples = this.buffer.length;

      // shiftBits needs to be 8 if 8 bit selected and 16 if 16 bit selected
      final int shiftBits = LogicSnifferDevice.this.config.getRLEDataWidth();
      final boolean ddrMode = LogicSnifferDevice.this.config.isDoubleDataRateEnabled();

      for ( int i = 0; i < samples; i++ )
      {
        final int sampleValue = this.buffer[i];
        final int normalizedSampleValue = normalizeSampleValue( sampleValue );

        // if a count just add it to the time
        if ( ( normalizedSampleValue & this.rleCountValue ) != 0 )
        {
          final int count;
          if ( ddrMode && ( i < ( samples - 1 ) ) )
          {
            // In case of "double data rate", the RLE-counts are encoded as 16-
            // resp. 32-bit values, so we need to take two samples for each
            // count (as they are 8- or 16-bits in DDR mode).
            // This should also solve issue #31...
            count = ( ( normalizedSampleValue & this.rleCountMask ) << shiftBits )
                | ( normalizeSampleValue( this.buffer[++i] ) & this.rleCountMask );
          }
          else
          {
            count = ( normalizedSampleValue & this.rleCountMask );
          }
          time += count;
        }
        else
        {
          // this is a data value only save data if different to last
          if ( sampleValue != oldSample )
          {
            // set the trigger position as a time value
            if ( ( i >= this.trigCount ) && ( rleTrigPos == 0 ) )
            {
              rleTrigPos = time;
            }
            // add the read sample & add a timestamp value as well...
            this.callback.addValue( sampleValue, time );
            oldSample = sampleValue;
          }
          time++;
        }
      }

      // Take the last seen time value as "absolete" length of this trace...
      this.callback.ready( time, rleTrigPos - 1 );
    }

    /**
     * Normalizes the given sample value to mask out the unused channel groups
     * and get a sample value in the correct width.
     * 
     * @param aSampleValue
     *          the original sample to normalize.
     * @return the normalized sample value.
     */
    private int normalizeSampleValue( final int aSampleValue )
    {
      int groupCount = LogicSnifferDevice.this.config.getGroupCount();
      int compdata = 0;

      // to enable non contiguous channel groups
      // need to remove zero data from unused groups
      int indata = aSampleValue;
      for ( int j = 0, outcount = 0; j < groupCount; j++ )
      {
        if ( LogicSnifferDevice.this.config.isGroupEnabled( j ) )
        {
          compdata |= ( ( indata & 0xff ) << ( 8 * outcount++ ) );
        }
        indata >>= 8;
      }
      return compdata;
    }
  }

  /**
   * Denotes a sample processor, which performs a transformation function (such
   * as uncompressing) on a set of samples.
   */
  static interface SampleProcessor
  {
    /**
     * Processes the samples.
     */
    void process();
  }

  /**
   * Provides a callback for the processed samples.
   * 
   * @see SampleProcessor
   */
  static interface SampleProcessorCallback
  {
    /**
     * @param aSampleValue
     * @param aTimestamp
     */
    void addValue( final int aSampleValue, final long aTimestamp );

    /**
     * @param aAbsoluteLength
     * @param aTriggerPosition
     */
    void ready( final long aAbsoluteLength, final long aTriggerPosition );
  }

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
  /** ask the device to immediately return its RLE-encoded data. */
  private static final int CMD_RLE_FINISH_NOW = 0x05;

  // demultiplex
  private static final int FLAG_DEMUX = 0x00000001;
  // noise filter
  private static final int FLAG_FILTER = 0x00000002;
  // external trigger?
  private static final int FLAG_EXTERNAL = 0x00000040;
  // inverted
  private static final int FLAG_INVERTED = 0x00000080;
  // run length encoding
  private static final int FLAG_RLE = 0x00000100;
  private static final int FLAG_RLE_MODE_0 = 0x00000000;
  private static final int FLAG_RLE_MODE_1 = 0x00004000;
  private static final int FLAG_RLE_MODE_2 = 0x00008000;
  private static final int FLAG_RLE_MODE_3 = 0x0000C000;
  // Number Scheme
  private static final int FLAG_NUMBER_SCHEME = 0x00000200;
  // Testing mode
  private static final int FLAG_TEST_MODE = 0x00000400;

  private static final Logger LOG = Logger.getLogger( LogicSnifferDevice.class.getName() );

  // VARIABLES

  private final LogicSnifferConfig config;

  private StreamConnection connection;
  private DataInputStream inputStream;
  private DataOutputStream outputStream;
  private boolean attached;
  private int trigcount;

  private volatile boolean running;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferDevice instance.
   */
  public LogicSnifferDevice( final LogicSnifferConfig aConfig )
  {
    this.config = aConfig;
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
   * Returns wether or not the device is currently running. It is running, when
   * another thread is inside the run() method reading data from the serial
   * port.
   * 
   * @return <code>true</code> when running, <code>false</code> otherwise
   */
  public boolean isRunning()
  {
    return this.running;
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
    if ( !this.running )
    {
      attach();

      sendCommand( CMD_SELFTEST );
      // TODO read selftest result...

      detach();
    }
  }

  /**
   * Informs the thread in run() that it is supposed to stop reading data and
   * return.
   */
  public void stop()
  {
    if ( this.running )
    {
      if ( this.config.isRleEnabled() )
      {
        try
        {
          sendCommand( CMD_RLE_FINISH_NOW );
        }
        catch ( IOException exception )
        {
          if ( !HostUtils.handleInterruptedException( exception ) )
          {
            LOG.log( Level.WARNING, "RLE 'finish now' failed?!", exception );
          }
        }
      }
      else
      {
        try
        {
          resetDevice();
        }
        catch ( IOException exception )
        {
          if ( !HostUtils.handleInterruptedException( exception ) )
          {
            LOG.log( Level.WARNING, "Device reset failed?!", exception );
          }
        }
        finally
        {
          this.running = false;
        }
      }
    }
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
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  protected CapturedData doInBackground() throws Exception
  {
    LOG.info( "Starting capture ..." );

    if ( !attach() )
    {
      throw new IOException( "Unable to open port " + this.config.getPortName() + ". No specific reason..." );
    }

    this.running = true;

    try
    {
      // First try to find the logic sniffer itself...
      detectDevice();

      // check if data needs to be multiplexed
      final int channels = this.config.getChannelCount();
      if ( channels < 0 )
      {
        throw new IllegalStateException( "Internal error: did not obtain correct number of channels (" + channels
            + ")?!" );
      }

      final int samples = this.config.getSampleCount();
      if ( samples < 0 )
      {
        throw new IllegalStateException( "Internal error: did not obtain correct number of samples (" + samples + ")?!" );
      }

      // We need to read all samples first before doing any post-processing on
      // them...
      final int[] buffer = new int[samples];
      // configure device
      configureDevice();

      sendCommand( CMD_RUN );

      int sampleIdx = samples - 1;
      boolean waiting = ( sampleIdx >= 0 );

      // wait for first byte forever (trigger could cause long delay)
      while ( this.running && waiting )
      {
        try
        {
          buffer[sampleIdx] = readSample( channels );
          sampleIdx--;
          waiting = false;
        }
        catch ( final InterruptedException exception )
        {
          // When running, we simply have a timeout; this could be that the
          // trigger is not fired yet... We keep waiting...
          if ( !this.running )
          {
            // Make sure to handle IO-interrupted exceptions properly!
            if ( !HostUtils.handleInterruptedException( exception ) )
            {
              throw exception;
            }
          }
        }
      }

      // read all other samples
      try
      {
        for ( ; this.running && ( sampleIdx >= 0 ); sampleIdx-- )
        {
          buffer[sampleIdx] = readSample( channels );
          setProgress( 100 - ( 100 * sampleIdx ) / buffer.length );
        }
      }
      catch ( InterruptedException exception )
      {
        LOG.log( Level.INFO, "Capture interrupted! Only {0} samples read ...", Integer.valueOf( samples - sampleIdx ) );

        // Make sure the device is in a state were we can do something with
        // it after this method is completed...
        resetDevice();

        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          throw exception;
        }
      }
      finally
      {
        setProgress( 100 );
      }

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
      final long[] triggerPos = { CapturedData.NOT_AVAILABLE };
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
          if ( LogicSnifferDevice.this.config.isTriggerEnabled() )
          {
            triggerPos[0] = aTriggerPosition;
          }
        }
      };

      final SampleProcessor processor;
      if ( this.config.isRleEnabled() )
      {
        LOG.log( Level.INFO, "Decoding Run Length Encoded data, sample count: {0}", Integer.valueOf( samples ) );
        processor = new RleDecoder( buffer, this.trigcount, callback );
      }
      else
      {
        LOG.log( Level.INFO, "Decoding unencoded data, sample count: {0}", Integer.valueOf( samples ) );
        processor = new EqualityFilter( buffer, this.trigcount, callback );
      }

      // Process the actual samples...
      processor.process();

      return new CapturedDataImpl( values, timestamps, triggerPos[0], rate, channels,
          this.config.getEnabledChannelsMask(), absoluteLength[0] );
    }
    finally
    {
      detach();

      // We're done; let's wrap it up...
      this.running = false;
    }
  }

  /**
   * Queries for a connector service to craft a connection for a given serial
   * port with a given baudrate.
   * 
   * @param aPortName
   *          the name of the port to create a connection for;
   * @param aPortRate
   *          the baudrate of the connection.
   * @return a connection capable of communicating with the requested serial
   *         device, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems, or in case the requested port is
   *           <em>not</em> a serial port.
   */
  protected abstract StreamConnection getConnection( final String aPortName, final int aPortRate ) throws IOException;

  /**
   * Finds the device profile manager.
   * 
   * @return a device profile manager instance, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the device profile manager could not be found/obtained.
   */
  protected abstract DeviceProfileManager getDeviceProfileManager();

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
  private boolean attach() throws IOException
  {
    final String portName = this.config.getPortName();
    final int baudrate = this.config.getBaudrate();
    final int openDelay = this.config.getOpenPortDelay();

    try
    {
      // Make sure we release the device if it was still attached...
      detach();

      LOG.log( Level.INFO, "Attaching to {0} @ {1}bps ...", new Object[] { portName, Integer.valueOf( baudrate ) } );

      this.connection = getConnection( portName, baudrate );
      if ( this.connection == null )
      {
        throw new IOException( "Failed to open a valid connection!" );
      }

      this.outputStream = this.connection.openDataOutputStream();
      this.inputStream = this.connection.openDataInputStream();

      // Some devices need some time to initialize after being opened for the
      // first time, see issue #34.
      if ( openDelay > 0 )
      {
        Thread.sleep( openDelay );
      }

      // We don't expect any data, so flush all data pending in the given
      // input stream. See issue #34.
      HostUtils.flushInputStream( this.inputStream );

      return this.attached = true;
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

    return false;
  }

  /**
   * Configures the OLS device by sending all configuration commands.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  private void configureDevice() throws IOException
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
      this.trigcount = ( size & 0xffff ) << 3 - ( ( size >> 16 ) & 0xffff ) << 3;
    }
    else
    {
      // 0x3fffc = 255Kb = the maximum size supported by the original SUMP
      // device...
      final int maxSize = 0x3fffc;
      size = ( ( stopCounter & maxSize ) << 14 ) | ( ( ( readCounter & maxSize ) >> 2 ) - 1 );
      // A better approximation of "(readCounter - stopCounter) - 2". This also
      // solves issue #31...
      this.trigcount = ( size & 0xffff ) << 2 - ( ( size >> 16 ) & 0xffff ) << 2;
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
    for ( int i = 0; i < 4; i++ )
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
      flags |= FLAG_TEST_MODE;
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
  private int configureTriggers() throws IOException
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
   * Detaches the currently attached port, if one exists. This will close the
   * serial port.
   */
  private void detach()
  {
    // We're definitely no longer attached...
    this.attached = false;

    if ( this.connection != null )
    {
      try
      {
        // try to make sure device is reset...
        if ( this.outputStream != null )
        {
          resetDevice();
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
          this.connection.close();
        }
        catch ( IOException exception )
        {
          LOG.log( Level.FINE, "Closing connection failed!", exception );
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
          LOG.log( Level.INFO, "Found Sump Logic Analyzer/LogicSniffer ...", Integer.toHexString( id ) );
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
        LOG.log( Level.INFO, "Found device '{0}'", metadata.getName() );
        LOG.log( Level.FINE, "Device metadata = \n{0}", metadata.toString() );

        final String name = metadata.getName();
        if ( name != null )
        {
          final DeviceProfileManager manager = getDeviceProfileManager();
          final DeviceProfile profile = manager.findProfile( name );
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
    if ( !this.attached )
    {
      throw new IllegalStateException( "Cannot fetch metadata from device: not attached!" );
    }

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
        catch ( final InterruptedException exception )
        {
          /* don't care */
          result = -1;

          // Make sure to handle IO-interrupted exceptions properly!
          if ( !HostUtils.handleInterruptedException( exception ) )
          {
            LOG.log( Level.INFO, "Port timeout!", exception );
          }
        }
      }
      while ( result > 0x00 );

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
   * Reads <code>channels</code> / 8 bytes from stream and compiles them into a
   * single integer.
   * 
   * @param aChannelCount
   *          number of channels to read (must be multiple of 8)
   * @return integer containing four bytes read
   * @throws IOException
   *           if stream reading fails
   */
  private int readSample( final int aChannelCount ) throws IOException, InterruptedException
  {
    int v, value = 0;
    // calculate number of groups expected, less than sChannelCount when Demux
    // set;
    // Wait until there's data available, otherwise we could get 'false'
    // timeouts; do not make the sleep too long, otherwise we might overflow our
    // receiver buffers...
    // if data not available here, client will stall until stop button pressed
    while ( ( this.inputStream.available() < this.config.getEnabledGroupCount() ) && !Thread.interrupted()
        && this.running )
    {
      Thread.sleep( 1L );
    }

    final int groupCount = aChannelCount / CapturedData.CHANNELS_PER_BLOCK;
    for ( int i = 0; i < groupCount; i++ )
    {
      v = 0; // in case the group is disabled, simply set it to zero...

      if ( this.config.isGroupEnabled( i ) )
      {
        v = this.inputStream.read();
        // Any timeouts/interrupts occurred?
        if ( v < 0 )
        {
          throw new InterruptedException( "Data readout interrupted: EOF." );
        }
        else if ( Thread.interrupted() )
        {
          throw new InterruptedException( "Data readout interrupted." );
        }
      }
      value |= v << ( 8 * i );
    }
    return value;
  }

  /**
   * Reads a zero-terminated ASCII-string from the current input stream.
   * 
   * @return the read string, can be empty but never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the string read;
   * @throws InterruptedException
   *           in case this thread was interrupted during the string read.
   */
  private String readString() throws IOException, InterruptedException
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
      else if ( Thread.interrupted() )
      {
        throw new InterruptedException( "Data readout interrupted!" );
      }
    }
    while ( read > 0x00 );

    return sb.toString();
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
  private void sendCommand( final int aOpcode ) throws IOException
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
  private void sendCommand( final int aOpcode, final int aData ) throws IOException
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
}
