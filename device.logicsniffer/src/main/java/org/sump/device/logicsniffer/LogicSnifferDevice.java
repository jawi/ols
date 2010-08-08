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


import gnu.io.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.devices.*;


/**
 * Device provides access to the physical logic analyzer device. It requires the
 * rxtx package from http://www.rxtx.org/ to access the serial port the analyzer
 * is connected to.
 */
public class LogicSnifferDevice implements Device
{
  // CONSTANTS

  /** Old SLA version, v0 - no longer supported. */
  private static final int    SLA_V0                 = 0x534c4130;
  /** Current SLA version, v1 - supported. */
  private static final int    SLA_V1                 = 0x534c4131;

  /** use internal clock */
  public final static int     CLOCK_INTERNAL         = 0;
  /** use external clock rising edge */
  public final static int     CLOCK_EXTERNAL_RISING  = 1;
  /** use external clock falling edge */
  public final static int     CLOCK_EXTERNAL_FALLING = 2;

  /** set trigger mask */
  private final static int    SETTRIGMASK            = 0xc0;
  /** set trigger value */
  private final static int    SETTRIGVAL             = 0xc1;
  /** set trigger configuration */
  private final static int    SETTRIGCFG             = 0xc2;
  /** set clock divider */
  private final static int    SETDIVIDER             = 0x80;
  /** set sample counters */
  private final static int    SETSIZE                = 0x81;
  /** set flags */
  private final static int    SETFLAGS               = 0x82;

  /** reset analyzer */
  private final static int    RESET                  = 0x00;
  /** arm trigger / run device */
  private final static int    RUN                    = 0x01;
  /** ask for device id */
  private final static int    ID                     = 0x02;
  /** continue data transmission to host */
  @SuppressWarnings( "unused" )
  private final static int    XON                    = 0x11;
  /** pause data transmission to host */
  @SuppressWarnings( "unused" )
  private final static int    XOFF                   = 0x13;

  private final static int    FLAG_DEMUX             = 0x00000001;                                            // demultiplex
  private final static int    FLAG_FILTER            = 0x00000002;                                            // noise
  // filter
  private final static int    FLAG_DISABLE_G0        = 0x00000004;                                            // disable
  // channel
  // group 0
  @SuppressWarnings( "unused" )
  private final static int    FLAG_DISABLE_G1        = 0x00000008;                                            // disable
  // channel
  // group 1
  private final static int    FLAG_DISABLE_G2        = 0x00000010;                                            // disable
  // channel
  // group 2
  @SuppressWarnings( "unused" )
  private final static int    FLAG_DISABLE_G3        = 0x00000020;                                            // disable
  // channel
  // group 3
  private final static int    FLAG_EXTERNAL          = 0x00000040;                                            // disable
  // channel
  // group 3
  private final static int    FLAG_INVERTED          = 0x00000080;                                            // disable
  // channel
  // group 3

  private final static int    FLAG_RLE               = 0x00000100;                                            // run
  // length
  // encoding

  private final static int    FLAG_NUMBER_SCHEME     = 0x00000200;                                            // Number
  // Scheme
  private final static int    FLAG_TEST_MODE         = 0x00000400;                                            // Number
  // Scheme

  private final static int    TRIGGER_DELAYMASK      = 0x0000ffff;                                            // mask
  // for
  // delay
  // value
  private final static int    TRIGGER_LEVELMASK      = 0x00030000;                                            // mask
  // for
  // level
  // value
  private final static int    TRIGGER_CHANNELMASK    = 0x01f00000;                                            // mask
  // for
  // level
  // value
  private final static int    TRIGGER_SERIAL         = 0x04000000;                                            // trigger
  // operates
  // in
  // serial mode
  private final static int    TRIGGER_CAPTURE        = 0x08000000;                                            // trigger
  // will
  // start
  // capture when
  // fired

  final static int CLOCK = 100000000; // device clock in Hz
  private final static int    TRIGGER_STAGES         = 4;                                                     // number
  // of
  // trigger
  // stages

  private static final Logger LOG                    = Logger.getLogger( LogicSnifferDevice.class.getName() );

  // VARIABLES

  private SerialPort          port;
  private InputStream         inputStream;
  private OutputStream        outputStream;
  private volatile boolean    running;
  // private int percentageDone;
  private int                 clockSource;
  private boolean             demux;
  private boolean             filterEnabled;
  private boolean             triggerEnabled;
  private boolean             rleEnabled;
  private boolean             altNumberSchemeEnabled;
  private boolean             testModeEnabled;
  private final int           triggerMask[];
  private final int           triggerValue[];
  private final int           triggerConfig[];
  private int                 enabledChannels;
  private final boolean       enabledGroups[];
  private int                 divider;
  private int                 size;
  private double              ratio;

  // CONSTRUCTORS

  /**
   * Creates a device object.
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

    stop();

    this.port = null;
  }

  // METHODS

  /**
   * Gets a string array containing the names all available serial ports.
   * 
   * @return array containing serial port names
   */
  @SuppressWarnings( "unchecked" )
  public static String[] getPorts()
  {
    final Enumeration<CommPortIdentifier> portIdentifiers = CommPortIdentifier.getPortIdentifiers();
    final LinkedList<String> portList = new LinkedList<String>();
    CommPortIdentifier portId = null;

    while ( portIdentifiers.hasMoreElements() )
    {
      portId = portIdentifiers.nextElement();
      if ( portId.getPortType() == CommPortIdentifier.PORT_SERIAL )
      {
        portList.addLast( portId.getName() );
      }
    }

    return ( portList.toArray( new String[portList.size()] ) );
  }

  /**
   * Attaches the given serial port to the device object. The method will try
   * to open the port.
   * <p>
   * A return value of <code>true</code> does not guarantee that a logic analyzer is actually attached to the port.
   * <p>
   * If the device is already attached to a port this port will be detached automatically. It is therefore not necessary
   * to manually call <code>detach()</code> before reattaching.
   * 
   * @param aPortName
   *          the name of the port to open
   * @param aPortRate
   *          transfer rate to use (bps)
   * @return <code>true</code> when the port has been assigned successfully; <code>false</code> otherwise.
   */
  @SuppressWarnings( "unchecked" )
  public boolean attach( final String aPortName, final int aPortRate )
  {
    final Enumeration<CommPortIdentifier> portList = CommPortIdentifier.getPortIdentifiers();
    CommPortIdentifier portId = null;
    boolean found = false;

    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Attaching to " + aPortName + " @ " + aPortRate + "bps" );
    }

    try
    {
      detach();

      while ( !found && portList.hasMoreElements() )
      {
        portId = portList.nextElement();

        if ( portId.getPortType() == CommPortIdentifier.PORT_SERIAL )
        {
          if ( portId.getName().equals( aPortName ) )
          {
            found = true;
          }
        }
      }

      if ( found )
      {
        this.port = ( SerialPort )portId.open( "Logic Analyzer Client", 1000 );

        this.port.setSerialPortParams( aPortRate, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE );
        this.port.setFlowControlMode( SerialPort.FLOWCONTROL_XONXOFF_IN );
        this.port.disableReceiveFraming();
        this.port.enableReceiveTimeout( 250 );

        this.outputStream = this.port.getOutputStream();
        this.inputStream = this.port.getInputStream();
      }
    }
    catch ( final Exception exception )
    {
      LOG.log( Level.ALL, "Failed to attach to " + aPortName, exception );
      return false;
    }
    return found;
  }

  /**
   * Detaches the currently attached port, if one exists. This will close the
   * serial port.
   */
  public void detach()
  {
    if ( this.port != null )
    {
      try
      {
        // try to make sure device is reset (see run() for loop
        // explanation)
        if ( this.outputStream != null )
        {
          for ( int i = 0; i < 5; i++ )
          {
            sendCommand( RESET );
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
      { /* don't care */
        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.log( Level.FINE, "Detaching failed!", exception );
        }
      }
      finally
      {
        this.port.close();
        this.port = null;
      }
    }
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
   * @return the clock source currently used as defined by the CLOCK_
   *         properties
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
   * @return <code>true</code> when noise filter is available, <code>false</code> otherwise
   */
  public boolean isFilterAvailable()
  {
    return ( !this.demux && ( this.clockSource == CLOCK_INTERNAL ) );
  }

  /**
   * Returns wether or not the noise filter is enabled.
   * 
   * @return <code>true</code> when noise filter is enabled, <code>false</code> otherwise
   */
  public boolean isFilterEnabled()
  {
    return ( this.filterEnabled );
  }

  /**
   * Returns wether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled, <code>false</code> otherwise
   */
  public boolean isRleEnabled()
  {
    return ( this.rleEnabled );
  }

  /**
   * Returns wether or not the device is currently running. It is running,
   * when another thread is inside the run() method reading data from the
   * serial port.
   * 
   * @return <code>true</code> when running, <code>false</code> otherwise
   */
  public boolean isRunning()
  {
    return ( this.running );
  }

  /**
   * Returns wether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled, <code>false</code> otherwise
   */
  public boolean isTestModeEnabled()
  {
    return ( this.testModeEnabled );
  }

  /**
   * Returns wether or not the trigger is enabled.
   * 
   * @return <code>true</code> when trigger is enabled, <code>false</code> otherwise
   */
  public boolean isTriggerEnabled()
  {
    return ( this.triggerEnabled );
  }

  /**
   * Sends the configuration to the device, starts it, reads the captured data
   * and returns a CapturedData object containing the data read as well as
   * device configuration information.
   * 
   * @return captured data
   * @throws IOException
   *           when writing to or reading from device fails
   * @throws InterruptedException
   *           if a read time out occurs after trigger match or stop() was
   *           called before trigger match
   */
  public CapturedDataImpl run( final ProgressCallback aCallback ) throws IOException, InterruptedException
  {
    this.running = true;

    // First try to find the logic sniffer itself...
    detectDevice();

    // configure device
    final int stopCounter = ( int )( this.size * this.ratio );
    final int readCounter = this.size;

    final int flags = configureDevice( stopCounter, readCounter );

    sendCommand( SETFLAGS, flags );
    sendCommand( RUN );

    // check if data needs to be multiplexed
    int channels;
    int samples;
    if ( this.demux && ( this.clockSource == CLOCK_INTERNAL ) )
    {
      channels = 16;
      samples = ( readCounter & 0xffff8 );
    }
    else
    {
      channels = 32;
      samples = ( readCounter & 0xffffc );
    }

    int[] buffer = new int[samples];

    // wait for first byte forever (trigger could cause long delay)
    for ( boolean wait = true; wait == true; )
    {
      try
      {
        buffer[samples - 1] = readSample( channels );
        wait = false;
      }
      catch ( final InterruptedException exception )
      {
        // When running, we simply have a timeout; this could be that the
        // trigger is not fired yet... We keep waiting...
        if ( !this.running )
        {
          // Make sure the thread's administration is correctly updated...
          Thread.currentThread().interrupt();
          throw exception;
        }
      }
    }

    // read all other samples
    try
    {
      for ( int i = samples - 2; i >= 0; i-- )
      {
        buffer[i] = readSample( channels );
        if ( aCallback != null )
        {
          aCallback.updateProgress( 100 - ( 100 * i ) / buffer.length );
        }
      }
    }
    finally
    {
      if ( aCallback != null )
      {
        aCallback.updateProgress( 100 );
      }
    }

    int rleTrigPos = 0;
    if ( this.rleEnabled )
    {
      if ( LOG.isLoggable( Level.FINE ) )
      {
        LOG.fine( "Run Length decode, samples: " + Integer.toString( samples ) );
      }

      final List<Integer> decodedBuffer = new ArrayList<Integer>( buffer.length );

      for ( int i = 0; i < samples; i++ )
      {
        if ( ( buffer[i] & 0x80000000 ) != 0 )
        {
          // This is a "count"
          if ( i == 0 )
          {
            // If the first sample is count, skip it.
            continue;
          }
          final int count = 0x7FFFFFFF & buffer[i];
          for ( int j = 0; j < count; j++ )
          {
            decodedBuffer.add( buffer[i - 1] );
          }
        }
        else
        {
          if ( ( i >= stopCounter - 2 ) && ( rleTrigPos == 0 ) )
          {
            rleTrigPos = decodedBuffer.size();
          }
          decodedBuffer.add( buffer[i] );
        }
      }
      // Copy decoded data to buffer.
      // TODO: Optimize this so it is done in place...
      buffer = new int[decodedBuffer.size()];
      for ( int i = 0; i < decodedBuffer.size(); i++ )
      {
        buffer[i] = decodedBuffer.get( i );
      }
    }

    // collect additional information for CapturedData
    int pos = CapturedData.NOT_AVAILABLE;
    if ( this.triggerEnabled )
    {
      if ( !this.rleEnabled )
      {
        pos = readCounter - stopCounter - 3 - ( 4 / ( this.divider + 1 ) ) - ( this.demux ? 5 : 0 );
      }
      else
      {
        pos = rleTrigPos - 1;
      }
    }

    int rate = CapturedData.NOT_AVAILABLE;
    if ( this.clockSource == CLOCK_INTERNAL )
    {
      rate = this.demux ? 2 * CLOCK / ( this.divider + 1 ) : CLOCK / ( this.divider + 1 );
    }

    return new CapturedDataImpl( buffer, pos, rate, channels, this.enabledChannels );
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
   *          <code>true</code> enables the noise filter, <code>false</code> disables it.
   */
  public void setFilterEnabled( final boolean enable )
  {
    this.filterEnabled = enable;
  }

  /**
   * Configures the given trigger stage in parallel mode. Currenty the trigger
   * has four stages (0-3).
   * <p>
   * In mask and value each bit of the integer parameters represents one channel. The LSB represents channel 0, the MSB
   * channel 31.
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the trigger level is 0.
   * 
   * @param stage
   *          trigger stage to write mask und value to
   * @param mask
   *          bit map defining which channels to watch
   * @param value
   *          bit map defining what value to wait for on watched channels
   * @param level
   *          trigger level at which the trigger will be armed (0 =
   *          immediatly)
   * @param delay
   *          delay in samples to wait in between match and fire
   * @param startCapture
   *          if <code>true</code> that capture when trigger fires,
   *          otherwise only triggel level will increase
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
   *          <code>true</code> enables the RLE, <code>false</code> disables
   *          it.
   */
  public void setRleEnabled( final boolean enable )
  {
    this.rleEnabled = enable;
  }

  /**
   * Configures the given trigger stage in serial mode. Currenty the trigger
   * has four stages (0-3).
   * <p>
   * In mask and value each bit of the integer parameters represents one sample. The LSB represents the oldest sample
   * not yet shifted out, the MSB the most recent. (The trigger compares to a 32bit shift register that is shifted by
   * one for each sample.)
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the trigger level is 0.
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
   *          trigger level at which the trigger will be armed (0 =
   *          immediatly)
   * @param delay
   *          delay in samples to wait in between match and fire
   * @param startCapture
   *          if <code>true</code> that capture when trigger fires,
   *          otherwise only triggel level will increase
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
   *          <code>true</code> enables the RLE, <code>false</code> disables
   *          it.
   */
  public void setTestModeEnabled( final boolean enable )
  {
    this.testModeEnabled = enable;
  }

  /**
   * Sets wheter or not to enable the trigger.
   * 
   * @param enable
   *          <code>true</code> enables the trigger, <code>false</code> disables it.
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
  }

  /**
   * @param aStopCounter
   * @param aReadCounter
   * @return
   * @throws IOException
   */
  int configureDevice( final int aStopCounter, final int aReadCounter ) throws IOException
  {
    int effectiveStopCounter;
    if ( this.triggerEnabled )
    {
      for ( int i = 0; i < TRIGGER_STAGES; i++ )
      {
        sendCommand( SETTRIGMASK + 4 * i, this.triggerMask[i] );
        sendCommand( SETTRIGVAL + 4 * i, this.triggerValue[i] );
        sendCommand( SETTRIGCFG + 4 * i, this.triggerConfig[i] );
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
    sendCommand( SETDIVIDER, this.divider );

    int flags = 0;
    if ( ( this.clockSource == CLOCK_EXTERNAL_RISING ) || ( this.clockSource == CLOCK_EXTERNAL_FALLING ) )
    {
      flags |= FLAG_EXTERNAL;
      if ( this.clockSource == CLOCK_EXTERNAL_FALLING )
      {
        flags |= FLAG_INVERTED;
      }
    }

    if ( this.demux && ( this.clockSource == CLOCK_INTERNAL ) )
    {
      flags |= FLAG_DEMUX;
      for ( int i = 0; i < 2; i++ )
      {
        if ( !this.enabledGroups[i] )
        {
          flags |= FLAG_DISABLE_G0 << i;
          flags |= FLAG_DISABLE_G2 << i;
        }
      }
      sendCommand( SETSIZE, ( ( ( effectiveStopCounter - 8 ) & 0x7fff8 ) << 13 )
          | ( ( ( aReadCounter & 0x7fff8 ) >> 3 ) - 1 ) );
    }
    else
    {
      if ( this.filterEnabled && isFilterAvailable() )
      {
        flags |= FLAG_FILTER;
      }

      for ( int i = 0; i < 4; i++ )
      {
        if ( !this.enabledGroups[i] )
        {
          flags |= FLAG_DISABLE_G0 << i;
        }
      }

      sendCommand( SETSIZE, ( ( ( effectiveStopCounter - 4 ) & 0x3fffc ) << 14 )
          | ( ( ( aReadCounter & 0x3fffc ) >> 2 ) - 1 ) );
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

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "Flags: " + Integer.toString( flags, 2 ) );
    }
    return flags;
  }

  /**
   * Tries to detect the LogicSniffer device.
   * 
   * @throws IOException
   *           in case the device could not be found, or in case of any other I/O problem.
   */
  final void detectDevice() throws IOException
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
        sendCommand( RESET );
      }

      // check if device is ready
      sendCommand( ID );

      try
      {
        id = readInteger();

        if ( LOG.isLoggable( Level.INFO ) )
        {
          if ( id == SLA_V0 )
          {
            LOG.info( "Found Sump Logic Analyzer (0x" + Integer.toHexString( id ) + ") ..." );
          }
          else if ( id == SLA_V1 )
          {
            LOG.info( "Found Sump Logic Analyzer/LogicSniffer (0x" + Integer.toHexString( id ) + ") ..." );
          }
          else
          {
            LOG.info( "Found unknown device (0x" + Integer.toHexString( id ) + ") ..." );
          }
        }
      }
      catch ( final IOException exception )
      {
        /* don't care */
        id = -1;

        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.log( Level.FINE, "I/O exception", exception );
        }
      }
      catch ( final InterruptedException exception )
      {
        /* don't care */
        id = -1;

        if ( LOG.isLoggable( Level.FINE ) )
        {
          LOG.log( Level.FINE, "Port timeout!", exception );
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
   * Reads a integer (32bits) from stream and compiles them into a single
   * integer.
   * 
   * @param channels
   *          number of channels to read (must be multiple of 8)
   * @return integer containing four bytes read
   * @throws IOException
   *           if stream reading fails
   */
  private int readInteger() throws IOException, InterruptedException
  {
    final byte bytes[] = new byte[4];

    final long until = System.currentTimeMillis() + 100L;
    while ( this.inputStream.available() < bytes.length )
    {
      if ( System.currentTimeMillis() > until )
      {
        throw new InterruptedException( "I/O time out!" );
      }

      try
      {
        TimeUnit.MICROSECONDS.sleep( 25L );
      }
      catch ( final InterruptedException ignore )
      {
        Thread.currentThread().interrupt();
        break;
      }
    }

    final int read = this.inputStream.read( bytes, 0, bytes.length );
    if ( ( read != bytes.length ) || Thread.interrupted() )
    {
      throw new InterruptedException( "Data readout interrupted!" );
    }
    // Craft a 32-bit value from the individual bytes...
    return ( bytes[3] << 24 ) | ( bytes[2] << 16 ) | ( bytes[1] << 8 ) | bytes[0];
  }

  /**
   * Reads <code>channels</code> / 8 bytes from stream and compiles them into
   * a single integer.
   * 
   * @param channels
   *          number of channels to read (must be multiple of 8)
   * @return integer containing four bytes read
   * @throws IOException
   *           if stream reading fails
   */
  private int readSample( final int channels ) throws IOException, InterruptedException
  {
    int v, value = 0;

    for ( int i = 0; i < channels / 8; i++ )
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
   * Sends a short command to the given stream.
   * This method is intended to be used for short commands, but can also be
   * called with long command opcodes if the data portion is to be set to 0.
   * 
   * @param opcode
   *          one byte operation code
   * @throws IOException
   *           if writing to stream fails
   */
  private void sendCommand( final int opcode ) throws IOException
  {
    final byte raw = ( byte )opcode;

    if ( LOG.isLoggable( Level.FINE ) )
    {
      String debugCmd = "Sending command: ";
      for ( int i = 7; i >= 0; i-- )
      {
        if ( ( raw & ( 1 << i ) ) != 0 )
        {
          debugCmd += "1";
        }
        else
        {
          debugCmd += "0";
        }
      }
      LOG.fine( debugCmd );
    }

    this.outputStream.write( raw );
    this.outputStream.flush();
  }

  /**
   * Sends a long command to the given stream.
   * 
   * @param opcode
   *          one byte operation code
   * @param data
   *          four byte data portion
   * @throws IOException
   *           if writing to stream fails
   */
  private void sendCommand( final int opcode, final int data ) throws IOException
  {
    final byte[] raw = new byte[5];
    int mask = 0xff;
    int shift = 0;

    raw[0] = ( byte )opcode;
    for ( int i = 1; i < 5; i++ )
    {
      raw[i] = ( byte )( ( data & mask ) >> shift );
      mask = mask << 8;
      shift += 8;
    }

    if ( LOG.isLoggable( Level.FINE ) )
    {
      String debugCmd = "Sending command: ";
      for ( int j = 0; j < 5; j++ )
      {
        for ( int i = 7; i >= 0; i-- )
        {
          if ( ( raw[j] & ( 1 << i ) ) != 0 )
          {
            debugCmd += "1";
          }
          else
          {
            debugCmd += "0";
          }
        }
        debugCmd += " ";
      }
      LOG.fine( debugCmd );
    }

    this.outputStream.write( raw );
    this.outputStream.flush();
  }
}
