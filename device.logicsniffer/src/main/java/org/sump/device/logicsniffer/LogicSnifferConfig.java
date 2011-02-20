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


import nl.lxtreme.ols.api.data.*;

import org.sump.device.logicsniffer.profile.DeviceProfile.*;


/**
 * @author jawi
 */
public final class LogicSnifferConfig
{
  // CONSTANTS

  /** The number of trigger stages. */
  private final static int TRIGGER_STAGES = 4;

  // mask for delay value
  private final static int TRIGGER_DELAYMASK = 0x0000ffff;
  // mask for level value
  private final static int TRIGGER_LEVELMASK = 0x00030000;
  // mask for channel value
  private final static int TRIGGER_CHANNELMASK = 0x01f00000;
  // trigger operates in serial mode
  private final static int TRIGGER_SERIAL = 0x04000000;
  // trigger will start capture when fired
  final static int TRIGGER_CAPTURE = 0x08000000;

  // VARIABLES

  private CaptureClockSource clockSource;
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

  private String portName;
  private int baudrate;
  private LogicSnifferMetadata metadata;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferConfig instance with all default settings.
   */
  public LogicSnifferConfig()
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
    setClockSource( CaptureClockSource.INTERNAL );
    this.divider = 0;
    this.ratio = 0.5;
    this.size = 512;
    this.enabledGroups = new boolean[4];
    setEnabledChannels( -1 ); // enable all channels

    this.metadata = new LogicSnifferMetadata();
  }

  // METHODS

  /**
   * Returns the baudrate in which to communicate with the device.
   * 
   * @return the baudrate, for example, 115200.
   */
  public int getBaudrate()
  {
    return this.baudrate;
  }

  /**
   * Returns the number of <em>available</em> channels in current configuration.
   * 
   * @return number of available channels, e.g., 8 or 16.
   */
  public int getChannelCount()
  {
    int channels;
    if ( this.demux && isInternalClock() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      channels = 16;
    }
    else
    {
      channels = 32;
    }

    if ( this.metadata != null )
    {
      channels = Math.min( channels, this.metadata.getProbeCount( channels ) );
    }

    return channels;
  }

  /**
   * Returns the current clock source.
   * 
   * @return the clock source currently used as defined by the CLOCK_ properties
   */
  public CaptureClockSource getClockSource()
  {
    return this.clockSource;
  }

  /**
   * Returns the divider, or the value used by the OLS device to take samples at
   * the requested rate.
   * 
   * @return the divider as integer.
   */
  public int getDivider()
  {
    return this.divider;
  }

  /**
   * Returns the currently enabled channels.
   * 
   * @return bitmask with enabled channels represented as 1
   */
  public int getEnabledChannelsMask()
  {
    return this.enabledChannels;
  }

  /**
   * Returns the number of channel groups (of 8 channels) that are enabled in
   * the capture.
   * 
   * @return the group count, >= 1 && < 4.
   */
  public int getEnabledGroupCount()
  {
    int cnt = 0;
    for ( boolean enabledGroup : this.enabledGroups )
    {
      if ( enabledGroup )
      {
        cnt++;
      }
    }

    if ( isDemuxEnabled() && isInternalClock() )
    {
      // In case the demux is enabled, only a maximum of two channel groups is
      // allowed...
      cnt = Math.min( 2, cnt );
    }

    return cnt;
  }

  /**
   * Returns the total number of channel groups.
   * 
   * @return a group count, >= 0 && < 4.
   */
  public int getGroupCount()
  {
    return getChannelCount() / CapturedData.CHANNELS_PER_BLOCK;
  }

  /**
   * Returns the number of available trigger stages.
   * 
   * @return number of available trigger stages, defaults to 4.
   */
  public int getMaxTriggerStages()
  {
    return TRIGGER_STAGES;
  }

  /**
   * Returns the metadata as obtained from the device.
   * 
   * @return the device metadata, might be <code>null</code>.
   */
  public LogicSnifferMetadata getMetadata()
  {
    return this.metadata;
  }

  /**
   * Returns the name of the port through which to communicate with the device.
   * 
   * @return the port name, e.g., "COM3", or "/dev/ttyACM0".
   */
  public String getPortName()
  {
    return this.portName;
  }

  /**
   * Returns the ratio of samples that is to be returned before the trigger and
   * after the trigger.
   * 
   * @return the ratio, between 0 and 1.
   */
  public double getRatio()
  {
    return this.ratio;
  }

  /**
   * The number of samples to read from the OLS device.
   * 
   * @return the read counter as integer value.
   * @see #getStopCounter()
   */
  public int getReadCounter()
  {
    return this.size;
  }

  /**
   * Returns the number of samples to be taken in current configuration.
   * 
   * @return number of samples, >= 0.
   */
  public int getSampleCount()
  {
    int samples;
    if ( isDemuxEnabled() && isInternalClock() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      samples = getReadCounter() & 0xffff8;
    }
    else
    {
      samples = getReadCounter() & 0xffffc;
    }

    return samples;
  }

  /**
   * Returns the rate in which samples are to be taken.
   * 
   * @return a sample rate, in Hertz (Hz).
   */
  public int getSampleRate()
  {
    int rate = CapturedData.NOT_AVAILABLE;
    if ( isInternalClock() )
    {
      if ( isDemuxEnabled() )
      {
        // The sample clock is 200MHz iso 100MHz...
        rate = ( int )( ( 2.0 * LogicSnifferDevice.CLOCK ) / ( getDivider() + 1 ) );
      }
      else
      {
        rate = ( int )( ( 1.0 * LogicSnifferDevice.CLOCK ) / ( getDivider() + 1 ) );
      }
    }
    return rate;
  }

  /**
   * Returns the "stop" counter, or the value on which the capture should be
   * stopped.
   * 
   * @return a stop counter value, >= 0 && < capture depth.
   * @see #getReadCounter()
   */
  public int getStopCounter()
  {
    return ( int )( getReadCounter() * getRatio() );
  }

  /**
   * Returns the configuration for a particular trigger stage.
   * 
   * @param aStage
   *          trigger stage to read mask from, >= 0 && <
   *          {@value #TRIGGER_STAGES}.
   * @return the trigger configuration as integer.
   */
  public int getTriggerConfig( final int aStage )
  {
    return this.triggerConfig[aStage];
  }

  /**
   * Returns the current trigger mask for a particular trigger stage.
   * 
   * @param aStage
   *          trigger stage to read mask from, >= 0 && <
   *          {@value #TRIGGER_STAGES}.
   * @return the current trigger mask, as integer.
   */
  public int getTriggerMask( final int aStage )
  {
    return this.triggerMask[aStage];
  }

  /**
   * Returns the current trigger value for a particular trigger stage.
   * 
   * @param aStage
   *          trigger stage to read mask from, >= 0 && <
   *          {@value #TRIGGER_STAGES}.
   * @return the current trigger value, as integer.
   */
  public int getTriggerValue( final int aStage )
  {
    return this.triggerValue[aStage];
  }

  /**
   * Returns whether the alternative number scheme for the probes is to be used.
   * 
   * @return <code>true</code> if the alternative number scheme is to be used,
   *         <code>false</code> otherwise.
   */
  public boolean isAltNumberSchemeEnabled()
  {
    return this.altNumberSchemeEnabled;
  }

  /**
   * Returns whether the demultiplexer is used. If the demultiplexer is enabled,
   * the sampling frequency is doubled to 200MHz.
   * 
   * @return <code>true</code> if the demultiplexer is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isDemuxEnabled()
  {
    return this.demux;
  }

  /**
   * Returns whether the sample clock is supplied externally.
   * 
   * @return <code>true</code> if an external sample clock is to be used,
   *         <code>false</code> if the internal sample clock is to be used.
   * @see #isInternalClock()
   */
  public boolean isExternalClock()
  {
    return ( this.clockSource == CaptureClockSource.EXTERNAL_FALLING )
        || ( this.clockSource == CaptureClockSource.EXTERNAL_RISING );
  }

  /**
   * Returns whether or not the noise filter can be used in the current
   * configuration.
   * 
   * @return <code>true</code> when noise filter is available,
   *         <code>false</code> otherwise.
   * @see #isFilterEnabled()
   */
  public boolean isFilterAvailable()
  {
    return !isDemuxEnabled() && isInternalClock();
  }

  /**
   * Returns whether or not the noise filter is enabled.
   * 
   * @return <code>true</code> when noise filter is enabled, <code>false</code>
   *         otherwise.
   * @see #isFilterAvailable()
   */
  public boolean isFilterEnabled()
  {
    return this.filterEnabled;
  }

  /**
   * Returns whether the probe-group with the given number is enabled.
   * 
   * @param aGroupNr
   *          the number of the probe-group, >= 0 && < 4.
   * @return <code>true</code> if the probe-group is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isGroupEnabled( final int aGroupNr )
  {
    return this.enabledGroups[aGroupNr];
  }

  /**
   * Returns whether the internal sample clock is used.
   * 
   * @return <code>true</code> if an internal sample clock is to be used,
   *         <code>false</code> if the external sample clock is to be used.
   * @see #isExternalClock()
   */
  public boolean isInternalClock()
  {
    return this.clockSource == CaptureClockSource.INTERNAL;
  }

  /**
   * Returns whether or not the run length encoding is enabled.
   * 
   * @return <code>true</code> when run length encoding is enabled,
   *         <code>false</code> otherwise
   */
  public boolean isRleEnabled()
  {
    return this.rleEnabled;
  }

  /**
   * Returns whether or not the test mode is enabled.
   * 
   * @return <code>true</code> when test mode is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isTestModeEnabled()
  {
    return this.testModeEnabled;
  }

  /**
   * Returns whether or not the triggers are to be used.
   * 
   * @return <code>true</code> when trigger are to be used, <code>false</code>
   *         otherwise.
   */
  public boolean isTriggerEnabled()
  {
    return this.triggerEnabled;
  }

  /**
   * Sets whether or not the alternative numbering scheme is to be used.
   * 
   * @param aEnable
   *          <code>true</code> if the alternative numbering scheme is to be
   *          used, <code>false</code> otherwise.
   */
  public void setAltNumberSchemeEnabled( final boolean aEnable )
  {
    this.altNumberSchemeEnabled = aEnable;
  }

  /**
   * Sets the communication speed with the device.
   * 
   * @param aBaudrate
   *          the baudrate to set.
   */
  public void setBaudrate( final int aBaudrate )
  {
    this.baudrate = aBaudrate;
  }

  /**
   * Sets the clock source to use.
   * 
   * @param aSource
   *          the clock source to use, cannot be <code>null</code>.
   */
  public void setClockSource( final CaptureClockSource aSource )
  {
    this.clockSource = aSource;
  }

  /**
   * Set enabled channels.
   * 
   * @param aMask
   *          bit map defining enabled channels
   */
  public void setEnabledChannels( final int aMask )
  {
    this.enabledChannels = aMask;
    // determine enabled groups
    for ( int i = 0; i < 4; i++ )
    {
      this.enabledGroups[i] = ( ( this.enabledChannels >> ( 8 * i ) ) & 0xff ) > 0;
    }
  }

  /**
   * Sets whether or not the noise filter is enabled.
   * 
   * @param aEnable
   *          <code>true</code> to enable the noise filter, <code>false</code>
   *          to disable it.
   */
  public void setFilterEnabled( final boolean aEnable )
  {
    this.filterEnabled = aEnable;
  }

  /**
   * Sets the device metadata.
   * 
   * @param aMetadata
   *          the metadata to set, cannot be <code>null</code>.
   */
  public void setMetadata( final LogicSnifferMetadata aMetadata )
  {
    this.metadata = aMetadata;
  }

  /**
   * Configures the given trigger stage in parallel mode. Currently, the trigger
   * has up to {@value #TRIGGER_STAGES} stages.
   * <p>
   * In mask and value each bit of the integer parameters represents one
   * channel. The LSB represents channel 0, the MSB channel 31.
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially the
   * trigger level is 0.
   * 
   * @param aStage
   *          trigger stage to write mask and value to
   * @param aMask
   *          bit map defining which channels to watch
   * @param aValue
   *          bit map defining what value to wait for on watched channels
   * @param level
   *          trigger level at which the trigger will be armed (0 = immediately)
   * @param aDelay
   *          delay in samples to wait in between match and fire
   * @param aStartCapture
   *          if <code>true</code> that capture when trigger fires, otherwise
   *          only trigger level will increase
   */
  public void setParallelTrigger( final int aStage, final int aMask, final int aValue, final int aLevel,
      final int aDelay, final boolean aStartCapture )
  {
    if ( isDemuxEnabled() )
    {
      this.triggerMask[aStage] = ( aMask & 0xFFFF ) | ( ( aMask & 0xFFFF ) << 16 );
      this.triggerValue[aStage] = ( aValue & 0xFFFF ) | ( ( aValue & 0xFFFF ) << 16 );
    }
    else
    {
      this.triggerMask[aStage] = aMask;
      this.triggerValue[aStage] = aValue;
    }

    this.triggerConfig[aStage] = 0;
    this.triggerConfig[aStage] |= aDelay & TRIGGER_DELAYMASK;
    this.triggerConfig[aStage] |= ( aLevel << 16 ) & TRIGGER_LEVELMASK;

    if ( aStartCapture )
    {
      this.triggerConfig[aStage] |= TRIGGER_CAPTURE;
    }
  }

  /**
   * Sets the name of the port to communicate with the device.
   * 
   * @param aPortName
   *          the port name to set, cannot be <code>null</code>.
   */
  public void setPortName( final String aPortName )
  {
    this.portName = aPortName;
  }

  /**
   * Sets the ratio for samples to read before and after started.
   * 
   * @param aRatio
   *          value between 0 and 1; 0 means all before start, 1 all after
   */
  public void setRatio( final double aRatio )
  {
    this.ratio = aRatio;
  }

  /**
   * Sets whether or not to enable the run length encoding.
   * 
   * @param enable
   *          <code>true</code> to enable the RLE, <code>false</code> to disable
   *          it.
   */
  public void setRleEnabled( final boolean enable )
  {
    this.rleEnabled = enable;
  }

  /**
   * Sets the number of samples to obtain when started.
   * 
   * @param aCount
   *          the number of samples to take, must be between 4 and 256*1024.
   */
  public void setSampleCount( final int aCount )
  {
    this.size = aCount;
  }

  /**
   * Set the sampling rate. All rates must be a divisor of 200.000.000. Other
   * rates will be adjusted to a matching divisor.
   * 
   * @param aRate
   *          the sampling rate in Hertz (Hz).
   */
  public void setSampleRate( final int aRate )
  {
    final int clock = LogicSnifferDevice.CLOCK;
    if ( aRate > clock )
    {
      this.demux = true;
      this.divider = ( int )( ( 2.0 * clock / aRate ) - 1 );
    }
    else
    {
      this.demux = false;
      this.divider = ( int )( ( 1.0 * clock / aRate ) - 1 );
    }
  }

  /**
   * Configures the given trigger stage in serial mode. Currently, the trigger
   * has up to {@value #TRIGGER_STAGES} stages.
   * <p>
   * In mask and value each bit of the integer parameters represents one sample.
   * The LSB represents the oldest sample not yet shifted out, the MSB the most
   * recent. (The trigger compares to a 32bit shift register that is shifted by
   * one for each sample.)
   * <p>
   * When a trigger fires, the trigger level will rise by one. Initially, the
   * trigger level is 0.
   * 
   * @param aStage
   *          trigger stage to write mask and value to
   * @param aChannel
   *          channel to attach trigger to
   * @param aMask
   *          bit map defining which channels to watch
   * @param aValue
   *          bit map defining what value to wait for on watched channels
   * @param aLevel
   *          trigger level at which the trigger will be armed (0 = immediately)
   * @param aDelay
   *          delay in samples to wait in between match and fire
   * @param aStartCapture
   *          if <code>true</code> that capture when trigger fires, otherwise
   *          only trigger level will increase
   */
  public void setSerialTrigger( final int aStage, final int aChannel, final int aMask, final int aValue,
      final int aLevel, final int aDelay, final boolean aStartCapture )
  {
    if ( isDemuxEnabled() )
    {
      this.triggerMask[aStage] = ( aMask & 0xFFFF ) | ( ( aMask & 0xFFFF ) << 16 );
      this.triggerValue[aStage] = ( aValue & 0xFFFF ) | ( ( aValue & 0xFFFF ) << 16 );
    }
    else
    {
      this.triggerMask[aStage] = aMask;
      this.triggerValue[aStage] = aValue;
    }

    this.triggerConfig[aStage] = TRIGGER_SERIAL;
    this.triggerConfig[aStage] |= aDelay & TRIGGER_DELAYMASK;
    this.triggerConfig[aStage] |= ( aLevel << 16 ) & TRIGGER_LEVELMASK;
    this.triggerConfig[aStage] |= ( aChannel << 20 ) & TRIGGER_CHANNELMASK;

    if ( aStartCapture )
    {
      this.triggerConfig[aStage] |= TRIGGER_CAPTURE;
    }
  }

  /**
   * Sets whether or not to enable the test mode.
   * 
   * @param enable
   *          <code>true</code> to enable the test mode, <code>false</code> to
   *          disable it.
   */
  public void setTestModeEnabled( final boolean enable )
  {
    this.testModeEnabled = enable;
  }

  /**
   * Sets whether or not to enable triggers.
   * 
   * @param enable
   *          <code>true</code> enables the use of trigger, <code>false</code>
   *          disables its use.
   */
  public void setTriggerEnabled( final boolean enable )
  {
    this.triggerEnabled = enable;
  }
}
