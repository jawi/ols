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
import java.util.concurrent.*;
import java.util.logging.*;

import javax.microedition.io.*;
import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;
import org.osgi.service.io.*;


/**
 * Device provides access to the physical logic analyzer device. It requires the
 * rxtx package from http://www.rxtx.org/ to access the serial port the analyzer
 * is connected to.
 */
public class LogicSnifferDevice extends SwingWorker<CapturedData, Integer>
{
  // CONSTANTS

  public static final String PROP_CAPTURE_ABORTED = "captureAborted";
  public static final String PROP_CAPTURE_DONE = "captureDone";
  public static final String PROP_CAPTURE_PROGRESS = "progress";

  /** Old SLA version, v0 (0x534c4130, or 0x30414c53) - no longer supported. */
  private static final int SLA_V0 = 0x30414c53;
  /** Current SLA version, v1 (0x534c4131, or 0x31414c53) - supported. */
  private static final int SLA_V1 = 0x31414c53;

  /** use internal clock */
  public final static int CLOCK_INTERNAL = 0;
  /** use external clock rising edge */
  public final static int CLOCK_EXTERNAL_RISING = 1;
  /** use external clock falling edge */
  public final static int CLOCK_EXTERNAL_FALLING = 2;

  /** set trigger mask */
  private final static int SETTRIGMASK = 0xc0;
  /** set trigger value */
  private final static int SETTRIGVAL = 0xc1;
  /** set trigger configuration */
  private final static int SETTRIGCFG = 0xc2;
  /** set clock divider */
  private final static int SETDIVIDER = 0x80;
  /** set sample counters */
  private final static int SETSIZE = 0x81;
  /** set flags */
  private final static int SETFLAGS = 0x82;

  /** reset analyzer */
  private final static int CMD_RESET = 0x00;
  /** arm trigger / run device */
  private final static int CMD_RUN = 0x01;
  /** ask for device id */
  private final static int CMD_ID = 0x02;
  /** ask for device self test. */
  @SuppressWarnings( "unused" )
  private final static int CMD_SELFTEST = 0x03;
  /** ask for device meta data. */
  private final static int CMD_METADATA = 0x04;

  // demultiplex
  private final static int FLAG_DEMUX = 0x00000001;
  // noise filter
  private final static int FLAG_FILTER = 0x00000002;
  // external trigger?
  private final static int FLAG_EXTERNAL = 0x00000040;
  // inverted
  private final static int FLAG_INVERTED = 0x00000080;
  // run length encoding
  private final static int FLAG_RLE = 0x00000100;

  // Number Scheme
  private final static int FLAG_NUMBER_SCHEME = 0x00000200;
  // Testing mode
  private final static int FLAG_TEST_MODE = 0x00000400;

  // mask for delay value
  private final static int TRIGGER_DELAYMASK = 0x0000ffff;
  // mask for level value
  private final static int TRIGGER_LEVELMASK = 0x00030000;
  // mask for channel value
  private final static int TRIGGER_CHANNELMASK = 0x01f00000;
  // trigger operates in serial mode
  private final static int TRIGGER_SERIAL = 0x04000000;
  // trigger will start capture when fired
  private final static int TRIGGER_CAPTURE = 0x08000000;

  final static int CLOCK = 100000000; // device clock in Hz
  // number of trigger stages
  private final static int TRIGGER_STAGES = 4;

  private static final Logger LOG = Logger.getLogger( LogicSnifferDevice.class.getName() );

  // VARIABLES

  private StreamConnection connection;
  private DataInputStream inputStream;
  private DataOutputStream outputStream;
  private volatile boolean running;
  private int clockSource;
  private boolean demux;
  private boolean filterEnabled;
  private boolean triggerEnabled;
  private boolean rleEnabled;
  private boolean altNumberSchemeEnabled;
  private boolean testModeEnabled;
  private final int triggerMask[];
  private final int triggerValue[];
  private final int triggerConfig[];
  private int enabledChannels;
  private final boolean enabledGroups[];
  private int divider;
  private int size;
  private double ratio;
  private boolean attached;

  private BundleContext bundleContext;

  // CONSTRUCTORS

  private String portName;

  // METHODS

  private int baudrate;

  /**
   * Creates a new LogicSnifferDevice instance.
   */
  public LogicSnifferDevice()
  {
    this.triggerMask = new int[4];
    this.triggerValue = new int[4];
    this.triggerConfig = new int[4];
    for ( int i = 0; i < TRIGGER_STAGES; i++ )
    {
      this.triggerMask[i] = 0;
      this.triggerValue[i] = 0;
      this.triggerConfig[i] = 0;
    }
    this.triggerEnabled = false;
    this.filterEnabled = false;
    this.demux = false;
    setClockSource( CLOCK_INTERNAL );
    this.divider = 0;
    this.ratio = 0.5;
    this.size = 512;
    this.enabledGroups = new boolean[4];
    setEnabledChannels( -1 ); // enable all channels
  }

  /**
   * Returns the number of available channels in current configuration.
   * 
   * @return number of available channels
   */
  public int getAvailableChannelCount()
  {
    if ( this.demux && ( this.clockSource == CLOCK_INTERNAL ) )
    {
      return ( 16 );
    }
    else
    {
      return ( 32 );
    }
  }

  /**
   * Returns the current clock source.
   * 
   * @return the clock source currently used as defined by the CLOCK_ properties
   */
  public int getClockSource()
  {
    return ( this.clockSource );
  }

  /**
   * Returns the currently enabled channels.
   * 
   * @return bitmask with enabled channels represented as 1
   */
  public int getEnabledChannels()
  {
    return ( this.enabledChannels );
  }

  /**
   * Get the maximum sampling rate available.
   * 
   * @return maximum sampling rate
   */
  public int getMaximumRate()
  {
    return ( 2 * CLOCK );
  }

  /**
   * Returns the current trigger mask.
   * 
   * @param stage
   *          trigger stage to read mask from
   * @return current trigger mask
   */
  public int getTriggerMask( final int stage )
  {
    return ( this.triggerMask[stage] );
  }

  /**
   * Returns the number of available trigger stages.
   * 
   * @return number of available trigger stages
   */
  public int getTriggerStageCount()
  {
    return ( TRIGGER_STAGES );
  }

  /**
   * Returns the current trigger value.
   * 
   * @param stage
   *          trigger stage to read value from
   * @return current trigger value
   */
  public int getTriggerValue( final int stage )
  {
    return ( this.triggerValue[stage] );
  }

  /**
   * Returns the current number scheme mask.
   * 
   * @return current number scheme mask
   */
  public boolean isAltNumberSchemeEnabled()
  {
    return ( this.altNumberSchemeEnabled );
  }

  /**
   * Returns wether or not the noise filter can be used in the current
   * configuration.
   * 
   * @return <code>true</code> when noise filter is available,
   *         <code>false</code> otherwise
   */
  public boolean isFilterAvailable()
  {
    return ( !this.demux && ( this.clockSource == CLOCK_INTERNAL ) );
  }

  /**
   * Returns wether or not the noise filter is enabled.
   * 
   * @return <code>true</code> when noise filter is enabled, <code>false</code>
   *         otherwise
   */
  public boolean isFilterEnabled()
  {
    return ( this.filterEnabled );
  }

  /**
   * Returns wether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled,
   *         <code>false</code> otherwise
   */
  public boolean isRleEnabled()
  {
    return ( this.rleEnabled );
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
    return ( this.running && !isCancelled() && !isDone() );
  }

  /**
   * Returns wether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled,
   *         <code>false</code> otherwise
   */
  public boolean isTestModeEnabled()
  {
    return ( this.testModeEnabled );
  }

  /**
   * Returns wether or not the trigger is enabled.
   * 
   * @return <code>true</code> when trigger is enabled, <code>false</code>
   *         otherwise
   */
  public boolean isTriggerEnabled()
  {
    return ( this.triggerEnabled );
  }

  /**
   * Sets the Number Scheme Mask
   * 
   * @param mask
   *          bit map defining number scheme.
   */
  public void setAltNumberSchemeEnabled( final boolean enable )
  {
    this.altNumberSchemeEnabled = enable;
  }

  /**
   * Sets the clock source to use.
   * 
   * @param source
   *          can be any CLOCK_ property of this class
   */
  public void setClockSource( final int source )
  {
    this.clockSource = source;
  }

  /**
   * Set enabled channels.
   * 
   * @param mask
   *          bit map defining enabled channels
   */
  public void setEnabledChannels( final int mask )
  {
    this.enabledChannels = mask;
    // determine enabled groups
    for ( int i = 0; i < 4; i++ )
    {
      this.enabledGroups[i] = ( ( this.enabledChannels >> ( 8 * i ) ) & 0xff ) > 0;
    }
  }

  /**
   * Sets wheter or not to enable the noise filter.
   * 
   * @param enable
   *          <code>true</code> enables the noise filter, <code>false</code>
   *          disables it.
   */
  public void setFilterEnabled( final boolean enable )
  {
    this.filterEnabled = enable;
  }

  /**
   * Configures the given trigger stage in parallel mode. Currenty the trigger
   * has four stages (0-3).
   * <p>
   * In mask and value each bit of the integer parameters represents one
   * channel. The LSB represents channel 0, the MSB channel 31.
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the
   * trigger level is 0.
   * 
   * @param stage
   *          trigger stage to write mask und value to
   * @param mask
   *          bit map defining which channels to watch
   * @param value
   *          bit map defining what value to wait for on watched channels
   * @param level
   *          trigger level at which the trigger will be armed (0 = immediatly)
   * @param delay
   *          delay in samples to wait in between match and fire
   * @param startCapture
   *          if <code>true</code> that capture when trigger fires, otherwise
   *          only triggel level will increase
   */
  public void setParallelTrigger( final int stage, final int mask, final int value, final int level, final int delay,
      final boolean startCapture )
  {
    if ( !this.demux )
    { // TODO: demux modification should be done on the fly in
      // run() and not with stored properties
      this.triggerMask[stage] = mask;
      this.triggerValue[stage] = value;
    }
    else
    {
      this.triggerMask[stage] = mask & 0xffff;
      this.triggerValue[stage] = value & 0xffff;
      this.triggerMask[stage] |= this.triggerMask[stage] << 16;
      this.triggerValue[stage] |= this.triggerValue[stage] << 16;
    }
    this.triggerConfig[stage] = 0;
    this.triggerConfig[stage] |= delay & TRIGGER_DELAYMASK;
    this.triggerConfig[stage] |= ( level << 16 ) & TRIGGER_LEVELMASK;
    if ( startCapture )
    {
      this.triggerConfig[stage] |= TRIGGER_CAPTURE;
    }
  }

  /**
   * @param aPortName
   * @param aBaudrate
   */
  public void setPortSettings( final String aPortName, final int aBaudrate )
  {
    this.portName = aPortName;
    this.baudrate = aBaudrate;
  }

  /**
   * Set the sampling rate. All rates must be a divisor of 200.000.000. Other
   * rates will be adjusted to a matching divisor.
   * 
   * @param aRate
   *          sampling rate in Hz
   */
  public void setRate( final int aRate )
  {
    if ( aRate > CLOCK )
    {
      this.demux = true;
      this.divider = ( 2 * CLOCK / aRate ) - 1;
    }
    else
    {
      this.demux = false;
      this.divider = ( CLOCK / aRate ) - 1;
    }
  }

  /**
   * Sets the ratio for samples to read before and after started.
   * 
   * @param ratio
   *          value between 0 and 1; 0 means all before start, 1 all after
   */
  public void setRatio( final double ratio )
  {
    this.ratio = ratio;
  }

  /**
   * Sets wheter or not to enable the run length encoding.
   * 
   * @param enable
   *          <code>true</code> enables the RLE, <code>false</code> disables it.
   */
  public void setRleEnabled( final boolean enable )
  {
    this.rleEnabled = enable;
  }

  /**
   * Configures the given trigger stage in serial mode. Currenty the trigger has
   * four stages (0-3).
   * <p>
   * In mask and value each bit of the integer parameters represents one sample.
   * The LSB represents the oldest sample not yet shifted out, the MSB the most
   * recent. (The trigger compares to a 32bit shift register that is shifted by
   * one for each sample.)
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the
   * trigger level is 0.
   * 
   * @param stage
   *          trigger stage to write mask und value to
   * @param channel
   *          channel to attach trigger to
   * @param mask
   *          bit map defining which channels to watch
   * @param value
   *          bit map defining what value to wait for on watched channels
   * @param level
   *          trigger level at which the trigger will be armed (0 = immediatly)
   * @param delay
   *          delay in samples to wait in between match and fire
   * @param startCapture
   *          if <code>true</code> that capture when trigger fires, otherwise
   *          only triggel level will increase
   */
  public void setSerialTrigger( final int stage, final int channel, final int mask, final int value, final int level,
      final int delay, final boolean startCapture )
  {
    if ( !this.demux )
    { // TODO: demux modification should be done on the fly in
      // run() and not with stored properties
      this.triggerMask[stage] = mask;
      this.triggerValue[stage] = value;
    }
    else
    {
      this.triggerMask[stage] = mask & 0xffff;
      this.triggerValue[stage] = value & 0xffff;
      this.triggerMask[stage] |= this.triggerMask[stage] << 16;
      this.triggerValue[stage] |= this.triggerValue[stage] << 16;
    }
    this.triggerConfig[stage] = 0;
    this.triggerConfig[stage] |= delay & TRIGGER_DELAYMASK;
    this.triggerConfig[stage] |= ( level << 16 ) & TRIGGER_LEVELMASK;
    this.triggerConfig[stage] |= ( channel << 20 ) & TRIGGER_CHANNELMASK;
    this.triggerConfig[stage] |= TRIGGER_SERIAL;
    if ( startCapture )
    {
      this.triggerConfig[stage] |= TRIGGER_CAPTURE;
    }
  }

  /**
   * Sets the number of samples to obtain when started.
   * 
   * @param size
   *          number of samples, must be between 4 and 256*1024
   */
  public void setSize( final int size )
  {
    this.size = size;
  }

  /**
   * Sets wheter or not to enable the run length encoding.
   * 
   * @param enable
   *          <code>true</code> enables the RLE, <code>false</code> disables it.
   */
  public void setTestModeEnabled( final boolean enable )
  {
    this.testModeEnabled = enable;
  }

  /**
   * Sets wheter or not to enable the trigger.
   * 
   * @param enable
   *          <code>true</code> enables the trigger, <code>false</code> disables
   *          it.
   */
  public void setTriggerEnabled( final boolean enable )
  {
    this.triggerEnabled = enable;
  }

  /**
   * Informs the thread in run() that it is supposed to stop reading data and
   * return.
   */
  public void stop()
  {
    this.running = false;
    cancel( true /* mayInterruptIfRunning */);
  }

  /**
   * Sets the OSGi bundle context.
   * 
   * @param aBundleContext
   *          the bundle context to set, cannot be <code>null</code>.
   */
  final void setBundleContext( final BundleContext aBundleContext )
  {
    this.bundleContext = aBundleContext;
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

    if ( !attach( this.portName, this.baudrate ) )
    {
      throw new IOException( "Unable to open port " + this.portName + ". No specific reason..." );
    }

    try
    {
      if ( !this.attached )
      {
        throw new IllegalStateException( "Cannot run capture from device: not attached!" );
      }

      this.running = true;

      // First try to find the logic sniffer itself...
      detectDevice();

      final LogicSnifferMetadata metadata = getMetadata();
      // Log the read results...
      LOG.log( Level.FINE, "Metadata = \n{0}", metadata.toString() );

      final int deviceSize = metadata.getSampleMemoryDepth( this.size );

      final int stopCounter = ( int )( deviceSize * this.ratio );
      final int readCounter = deviceSize;

      // check if data needs to be multiplexed
      final int channels;
      final int samples;
      if ( this.demux && ( this.clockSource == CLOCK_INTERNAL ) )
      {
        // When the multiplexer is turned on, the upper two channel blocks are
        // disabled, leaving only 16 channels for capturing...
        channels = metadata.getProbeCount( 16 );
        samples = ( readCounter & 0xffff8 );
      }
      else
      {
        channels = metadata.getProbeCount( 32 );
        samples = ( readCounter & 0xffffc );
      }

      // We need to read all samples first before doing any post-processing on
      // them...
      final int[] buffer = new int[samples];

      // configure device
      configureDevice( stopCounter, readCounter );

      sendCommand( CMD_RUN );

      boolean waiting = true;
      int sampleIdx = samples - 1;

      // wait for first byte forever (trigger could cause long delay)
      while ( this.running && waiting )
      {
        try
        {
          buffer[sampleIdx] = readSample( channels );

          // Notify the EDT that there's possibly something to display...
          publish( buffer[sampleIdx] );

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

          // Notify the EDT that there's possibly something to display...
          publish( buffer[sampleIdx] );
        }
      }
      finally
      {
        setProgress( 100 );
      }

      final List<Integer> values = new ArrayList<Integer>();
      final List<Long> timestamps = new ArrayList<Long>();

      int rleTrigPos = 0;
      long absoluteLength = 0;

      // collect additional information for CapturedData
      int triggerPos = CapturedData.NOT_AVAILABLE;

      int rate = CapturedData.NOT_AVAILABLE;
      if ( this.clockSource == CLOCK_INTERNAL )
      {
        rate = this.demux ? 2 * CLOCK / ( this.divider + 1 ) : CLOCK / ( this.divider + 1 );
      }

      if ( this.rleEnabled )
      {
        LOG.log( Level.FINE, "Decoding Run Length Encoded data, sample count: {0}", samples );

        int old = buffer[0];
        long time = 0;
        for ( int i = 0; i < samples; i++ )
        {
          if ( ( buffer[i] & 0x80000000 ) != 0 )
          {
            // This is a "count"
            if ( ( old & 0x80000000 ) != 0 )
            {
              // Skip the first part of the stream if it is composed from
              // repeated repeat-counts.
              old = buffer[i];
              LOG.log( Level.INFO, "Duplicate RLE count seen of {0} vs {1}!", new Object[] {
                  ( buffer[i] & 0x7FFFFFFF ), ( old & 0x7FFFFFFF ) } );
              continue;
            }

            final int count = ( buffer[i] & 0x7FFFFFFF );
            // simple increase the time value at which the next sample will
            // occur...
            LOG.log( Level.FINE, "RLE count seen of {0} times {1}.", new Object[] { count, buffer[i - 1] } );
            time += count;
          }
          else
          {
            if ( ( i >= stopCounter - 2 ) && ( rleTrigPos == 0 ) )
            {
              rleTrigPos = values.size();
            }

            // add the read sample & add a timestamp value as well...
            values.add( buffer[i] );
            timestamps.add( time++ );

            old = buffer[i];
          }
        }

        // Take the last seen time value as "absolete" length of this trace...
        absoluteLength = time;

        if ( this.triggerEnabled )
        {
          triggerPos = rleTrigPos - 1;
        }
      }
      else
      {
        LOG.log( Level.FINE, "Decoding unencoded data, sample count: {0}", samples );

        for ( int i = 0; i < samples; i++ )
        {
          values.add( buffer[i] );
          timestamps.add( ( long )i );
        }

        // Take the number of samples as "absolute" length of this trace...
        absoluteLength = samples;

        if ( this.triggerEnabled )
        {
          // TODO what the f*ck is this doing???
          triggerPos = readCounter - stopCounter - 3 - ( 4 / ( this.divider + 1 ) ) - ( this.demux ? 5 : 0 );
        }
      }

      return new CapturedDataImpl( values, timestamps, triggerPos, rate, channels, this.enabledChannels, absoluteLength );
    }
    finally
    {
      detach();
    }
  }

  /**
   * @see javax.swing.SwingWorker#done()
   */
  @Override
  protected void done()
  {
    CapturedData data = null;
    String abortReason = null;

    try
    {
      data = get();
    }
    catch ( CancellationException exception )
    {
      abortReason = ""; // simply canceled by user...
      stop();
    }
    catch ( ExecutionException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception.getCause() ) )
      {
        abortReason = exception.getCause().getMessage();
        stop();
      }
    }
    catch ( InterruptedException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        abortReason = exception.getMessage();
        stop();
      }
    }

    // Report the result back to the given callback...
    if ( isCancelled() || ( abortReason != null ) )
    {
      firePropertyChange( PROP_CAPTURE_ABORTED, null, abortReason );
    }
    else if ( isDone() )
    {
      firePropertyChange( PROP_CAPTURE_DONE, null, data );
    }
    else
    {
      if ( LOG.isLoggable( Level.WARNING ) )
      {
        LOG.warning( "Internal state error: not done nor cancelled?!" );
      }
    }
  }

  /**
   * Processes the given chunk of samples.
   * 
   * @param aSamples
   *          the list with samples to process, cannot be <code>null</code>.
   * @see javax.swing.SwingWorker#process(java.util.List)
   */
  @Override
  protected void process( final List<Integer> aSamples )
  {
    // TODO Auto-generated method stub
    super.process( aSamples );
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#attach(java.lang.String, int)
   */
  private boolean attach( final String aPortName, final int aPortRate ) throws IOException
  {
    try
    {
      detach();

      LOG.log( Level.INFO, "Attaching to {0} @ {1}bps ...", new Object[] { aPortName, aPortRate } );

      this.connection = getConnection( aPortName, aPortRate );
      if ( this.connection != null )
      {
        this.outputStream = this.connection.openDataOutputStream();
        this.inputStream = this.connection.openDataInputStream();

        return this.attached = true;
      }

    }
    catch ( final Exception exception )
    {
      LOG.log( Level.WARNING, "Failed to open/use {0}! Possible reason: {1}",
          new Object[] { aPortName, exception.getMessage() } );
      LOG.log( Level.FINE, "Detailed stack trace:", exception );

      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        throw new IOException( "Failed to open/use " + aPortName + "! Possible reason: " + exception.getMessage() );
      }
    }

    return false;
  }

  /**
   * @param aStopCounter
   * @param aReadCounter
   * @return
   * @throws IOException
   */
  private int configureDevice( final int aStopCounter, final int aReadCounter ) throws IOException
  {
    final int effectiveStopCounter = configureTriggers( aStopCounter, aReadCounter );

    int flags = 0;
    if ( ( this.clockSource == CLOCK_EXTERNAL_RISING ) || ( this.clockSource == CLOCK_EXTERNAL_FALLING ) )
    {
      flags |= FLAG_EXTERNAL;
      if ( this.clockSource == CLOCK_EXTERNAL_FALLING )
      {
        flags |= FLAG_INVERTED;
      }
    }

    // determine which channel groups are to be disabled...
    int enabledChannelGroups = 0;
    for ( int i = 0; i < 4; i++ )
    {
      if ( this.enabledGroups[i] )
      {
        enabledChannelGroups |= ( 1 << i );
      }
    }
    flags |= ~( enabledChannelGroups << 2 ) & 0x3c;

    final int size;
    if ( this.demux && ( this.clockSource == CLOCK_INTERNAL ) )
    {
      flags |= FLAG_DEMUX;
      // if the demux bit is set, the filter flag *must* be clear...
      flags &= ~FLAG_FILTER;

      size = ( ( ( effectiveStopCounter - 8 ) & 0x7fff8 ) << 13 ) | ( ( ( aReadCounter & 0x7fff8 ) >> 3 ) - 1 );
    }
    else
    {
      if ( this.filterEnabled && isFilterAvailable() )
      {
        flags |= FLAG_FILTER;
        // if the filter bit is set, the filter flag *must* be clear...
        flags &= ~FLAG_DEMUX;
      }

      size = ( ( ( effectiveStopCounter - 4 ) & 0x3fffc ) << 14 ) | ( ( ( aReadCounter & 0x3fffc ) >> 2 ) - 1 );
    }

    if ( this.rleEnabled )
    {
      flags |= FLAG_RLE;
    }

    if ( this.altNumberSchemeEnabled )
    {
      flags |= FLAG_NUMBER_SCHEME;
    }

    if ( this.testModeEnabled )
    {
      flags |= FLAG_TEST_MODE;
    }

    LOG.log( Level.FINE, "Flags: 0b{0}", Integer.toBinaryString( flags ) );

    // set the sampling frequency...
    sendCommand( SETDIVIDER, this.divider );

    sendCommand( SETSIZE, size );

    sendCommand( SETFLAGS, flags );

    return flags;
  }

  /**
   * @param aStopCounter
   * @param aReadCounter
   * @return
   * @throws IOException
   */
  private int configureTriggers( final int aStopCounter, final int aReadCounter ) throws IOException
  {
    int effectiveStopCounter;
    if ( this.triggerEnabled )
    {
      for ( int i = 0; i < TRIGGER_STAGES; i++ )
      {
        final int indexMask = 4 * i;
        sendCommand( SETTRIGMASK | indexMask, this.triggerMask[i] );
        sendCommand( SETTRIGVAL | indexMask, this.triggerValue[i] );
        sendCommand( SETTRIGCFG | indexMask, this.triggerConfig[i] );
      }
      effectiveStopCounter = aStopCounter;
    }
    else
    {
      sendCommand( SETTRIGMASK, 0 );
      sendCommand( SETTRIGVAL, 0 );
      sendCommand( SETTRIGCFG, TRIGGER_CAPTURE );
      effectiveStopCounter = aReadCounter;
    }
    return effectiveStopCounter;
  }

  /**
   * Detaches the currently attached port, if one exists. This will close the
   * serial port.
   */
  private void detach()
  {
    if ( this.connection != null )
    {
      try
      {
        // try to make sure device is reset (see run() for loop explanation)
        if ( this.outputStream != null )
        {
          for ( int i = 0; i < 5; i++ )
          {
            sendCommand( CMD_RESET );
          }
          this.outputStream.flush();
          this.outputStream.close();
        }

        if ( this.inputStream != null )
        {
          this.inputStream.close();
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

          this.attached = false;
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
    int tries = 3;
    int id = -1;
    while ( ( tries-- >= 0 ) && ( id != SLA_V0 ) && ( id != SLA_V1 ) )
    {
      // Make sure nothing is left in our input buffer...
      flushInput();

      // send reset 5 times because in worst case first 4 are interpreted as
      // data of long command
      for ( int i = 0; i < 5; i++ )
      {
        sendCommand( CMD_RESET );
      }

      // check if device is ready
      sendCommand( CMD_ID );

      try
      {
        id = this.inputStream.readInt();

        if ( id == SLA_V0 )
        {
          LOG.log( Level.INFO, "Found Sump Logic Analyzer (0x{0}) ...", Integer.toHexString( id ) );
        }
        else if ( id == SLA_V1 )
        {
          LOG.log( Level.INFO, "Found Sump Logic Analyzer/LogicSniffer (0x{0}) ...", Integer.toHexString( id ) );
        }
        else
        {
          LOG.log( Level.INFO, "Found unknown device (0x{0}) ...", Integer.toHexString( id ) );
        }
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
  }

  /**
   * Flushes the input buffer from the serial port.
   * 
   * @throws IOException
   *           in case of I/O problems.
   */
  private void flushInput() throws IOException
  {
    if ( this.inputStream != null )
    {
      while ( this.inputStream.available() > 0 )
      {
        this.inputStream.read();
      }
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
  private StreamConnection getConnection( final String aPortName, final int aPortRate ) throws IOException
  {
    if ( this.bundleContext != null )
    {
      final ServiceReference serviceRef = this.bundleContext.getServiceReference( ConnectorService.class.getName() );
      if ( serviceRef != null )
      {
        final ConnectorService connectorService = ( ConnectorService )this.bundleContext.getService( serviceRef );

        try
        {
          final String portUri = String.format(
              "comm:%s;baudrate=%d;bitsperchar=8;parity=none;stopbits=1;flowcontrol=xon_xoff", aPortName, aPortRate );

          return ( StreamConnection )connectorService.open( portUri, ConnectorService.READ_WRITE, false /* timeouts */);
        }
        finally
        {
          // Release the connector service, to avoid possible resource leaks...
          this.bundleContext.ungetService( serviceRef );
        }
      }
    }

    return null;
  }

  /**
   * @see nl.lxtreme.ols.api.devices.Device#getMetadata()
   */
  private LogicSnifferMetadata getMetadata() throws IOException, IllegalStateException
  {
    if ( !this.attached )
    {
      throw new IllegalStateException( "Cannot fetch metadata from device: not attached!" );
    }

    // Make sure nothing is left in our input buffer...
    flushInput();

    // Ok; device appears to be good and willing to communicate; let's get its
    // metadata...
    sendCommand( CMD_METADATA );

    final LogicSnifferMetadata metadata = new LogicSnifferMetadata();

    int result = -1;
    do
    {
      try
      {
        result = this.inputStream.read();

        if ( result > 0 )
        {
          final int type = ( result & 0xE0 ) >> 5;
          if ( type == 0x00 )
          {
            // key value is a null-terminated string...
            final String value = readString();
            LOG.log( Level.FINE, "Read {0} -> \"{1}\"", new Object[] { result, value } );
            metadata.put( result, value );
          }
          else if ( type == 0x01 )
          {
            // key value is a 32-bit integer...
            final Integer value = this.inputStream.readInt();
            LOG.log( Level.FINE, "Read {0} -> {1} (32-bit)", new Object[] { result, value } );
            metadata.put( result, value );
          }
          else if ( type == 0x02 )
          {
            // key value is a 8-bit integer...
            final Integer value = this.inputStream.read();
            LOG.log( Level.FINE, "Read {0} -> {1} (8-bit)", new Object[] { result, value } );
            metadata.put( result, value );
          }
          else
          {
            LOG.log( Level.INFO, "Ignoring unknown type: {0}", type );
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

  /**
   * Reads <code>channels</code> / 8 bytes from stream and compiles them into a
   * single integer.
   * 
   * @param aChannels
   *          number of channels to read (must be multiple of 8)
   * @return integer containing four bytes read
   * @throws IOException
   *           if stream reading fails
   */
  private int readSample( final int aChannels ) throws IOException, InterruptedException
  {
    int v, value = 0;

    final int groupCount = ( int )Math.ceil( aChannels / 8.0 );
    for ( int i = 0; i < groupCount; i++ )
    {
      v = 0; // in case the group is disabled, simply set it to zero...

      if ( this.enabledGroups[i] )
      {
        v = this.inputStream.read();

        // Any timeouts/interrupts occurred?
        if ( ( v < 0 ) || Thread.interrupted() )
        {
          throw new InterruptedException( "Data readout interrupted." );
        }
      }

      value |= v << ( 8 * i );
    }

    return value;
  }

  /**
   * @return
   * @throws IOException
   * @throws InterruptedException
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
    if ( LOG.isLoggable( Level.FINE ) )
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
    if ( LOG.isLoggable( Level.FINE ) )
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
