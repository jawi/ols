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


import nl.lxtreme.ols.api.*;

import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.CaptureClockSource;
import org.sump.device.logicsniffer.protocol.*;


/**
 * Provides the configuration options for the LogicSniffer device.
 */
public final class LogicSnifferConfig
{
  // CONSTANTS

  // mask for delay value
  private final static int TRIGGER_DELAYMASK = 0x0000ffff;
  // mask for level value
  private final static int TRIGGER_LEVELMASK = 0x00030000;
  // mask for channel value
  private final static int TRIGGER_CHANNELMASK = 0x01f00000;
  // trigger operates in serial mode
  private final static int TRIGGER_SERIAL = 0x04000000;
  /** The number of trigger stages. */
  public final static int TRIGGER_STAGES = 4;
  // trigger will start capture when fired
  public final static int TRIGGER_CAPTURE = 0x08000000;

  static final int MIN_CHANNEL_GROUPS = 1;
  static final int MAX_CHANNEL_GROUPS_DDR = 2;

  // VARIABLES

  private CaptureClockSource clockSource;
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
  private int size;
  private double ratio;
  private int rleDataWidth;
  private DeviceProfile deviceProfile;
  private int sampleRate;
  private String connectionURI;

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
    setClockSource( CaptureClockSource.INTERNAL );
    this.ratio = 0.5;
    this.size = 512;
    this.enabledGroups = new boolean[] { true, true, true, true };
  }

  // METHODS

  /**
   * Returns the number of <em>available</em> channels in current configuration.
   * 
   * @return the total number of available channels, e.g., 8, 16, 24 or 32.
   */
  public int getChannelCount()
  {
    int channels = 32;
    if ( isDoubleDataRateEnabled() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      channels = 16;
    }

    if ( this.deviceProfile != null )
    {
      channels = Math.min( channels, this.deviceProfile.getChannelCount() );
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
   * Returns the (maximum) clockspeed the device runs at.
   * 
   * @return a clockspeed, in Hertz (Hz).
   */
  public int getClockspeed()
  {
    int result = SumpProtocolConstants.CLOCK;
    if ( this.deviceProfile != null )
    {
      result = this.deviceProfile.getClockspeed();
    }
    return result;
  }

  /**
   * Returns the current value of connectionURI.
   * 
   * @return the connectionURI
   */
  public String getConnectionURI()
  {
    return this.connectionURI;
  }

  /**
   * @return the deviceProfile
   */
  public DeviceProfile getDeviceProfile()
  {
    return this.deviceProfile;
  }

  /**
   * Returns the divider, or the value used by the OLS device to take samples at
   * the requested rate.
   * 
   * @return the divider as integer.
   * @see #setSampleRate(int)
   */
  public int getDivider()
  {
    double clock = getDividerClockspeed();
    if ( isDoubleDataRateEnabled() )
    {
      clock *= 2.0;
    }
    return ( int )Math.max( 0.0, ( ( clock / this.sampleRate ) - 1.0 ) );
  }

  /**
   * Returns the clockspeed used in the divider calculation.
   * 
   * @return a clockspeed, in Hertz (Hz), defaults to 100MHz.
   */
  public int getDividerClockspeed()
  {
    int result = SumpProtocolConstants.CLOCK;
    if ( this.deviceProfile != null )
    {
      result = this.deviceProfile.getDividerClockspeed();
    }
    return result;
  }

  /**
   * Returns the number of currently enabled channels.
   * 
   * @return a number of enabled channels, either 8, 16, 24 or 32.
   */
  public int getEnabledChannelsCount()
  {
    int result = getEnabledGroupCount() * 8;
    if ( this.deviceProfile != null )
    {
      // Make sure we adhere to the maximum number of channels of a device...
      result = Math.min( result, this.deviceProfile.getChannelCount() );
    }

    return result;
  }

  /**
   * Returns the mask of the currently enabled channels.
   * 
   * @return a bitmask with enabled channels represented as 1 integer
   *         representation (power of two), >= 0.
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
    // Important: #getGroupCount take care of DDR-mode for us, so we only have
    // to assume the enabled groups booleans are set symmetrically...
    for ( int i = getGroupCount() - 1; i >= 0; i-- )
    {
      if ( this.enabledGroups[i] )
      {
        cnt++;
      }
    }
    return cnt;
  }

  /**
   * Returns the total number of channel groups.
   * <p>
   * This method will divide the number of available channels by 8 (assuming a
   * group is <em>always</em> 8-bits/channels wide). If the channel count is
   * less than 8 this method will always yield 1. If double-data rate is
   * supported, then the maximum number of available groups is limited to 2.
   * </p>
   * 
   * @return a group count (zero-based), >= 1 && < 4.
   */
  public int getGroupCount()
  {
    int cnt = Math.max( MIN_CHANNEL_GROUPS, getChannelCount() / Ols.CHANNELS_PER_BLOCK );
    if ( isDoubleDataRateEnabled() )
    {
      // In case the demux is enabled, only a maximum of two channel groups is
      // allowed...
      cnt = Math.min( MAX_CHANNEL_GROUPS_DDR, cnt );
    }

    return cnt;
  }

  /**
   * Returns the number of available trigger stages.
   * 
   * @return number of available trigger stages, defaults to 4.
   */
  public int getMaxTriggerStages()
  {
    int result = TRIGGER_STAGES;
    if ( this.deviceProfile != null )
    {
      result = Math.min( result, this.deviceProfile.getTriggerStages() );
    }
    return result;
  }

  /**
   * @see DeviceProfile#getOpenPortDelay()
   */
  public int getOpenPortDelay()
  {
    if ( this.deviceProfile == null )
    {
      return 0;
    }
    return this.deviceProfile.getOpenPortDelay();
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
   * Returns the data width to use for RLE encoded samples.
   * 
   * @return a RLE data width, e.g., 8-, 16-, 24-, or 32-bits.
   */
  public int getRLEDataWidth()
  {
    return this.rleDataWidth;
  }

  /**
   * Returns the number of samples to be taken in current configuration.
   * 
   * @return number of samples, >= 0.
   */
  public int getSampleCount()
  {
    int samples;
    if ( isDoubleDataRateEnabled() )
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
    return this.sampleRate;
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
    if ( ( aStage < 0 ) || ( aStage >= TRIGGER_STAGES ) )
    {
      throw new IllegalArgumentException( "Invalid trigger stage: " + aStage + "!" );
    }
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
    if ( ( aStage < 0 ) || ( aStage >= TRIGGER_STAGES ) )
    {
      throw new IllegalArgumentException( "Invalid trigger stage: " + aStage + "!" );
    }
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
    if ( ( aStage < 0 ) || ( aStage >= TRIGGER_STAGES ) )
    {
      throw new IllegalArgumentException( "Invalid trigger stage: " + aStage + "!" );
    }
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
   * @see #setSampleRate(int)
   */
  public boolean isDoubleDataRateEnabled()
  {
    if ( !isInternalClock() || !isDoubleDataRateSupported() )
    {
      return false;
    }

    final int clock = getClockspeed();
    return ( this.sampleRate > clock );
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
   */
  public boolean isFilterAvailable()
  {
    boolean result = !isDoubleDataRateEnabled();
    if ( result && ( this.deviceProfile != null ) )
    {
      result = this.deviceProfile.isNoiseFilterSupported();
    }
    return result;
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
    if ( ( aGroupNr < 0 ) || ( aGroupNr >= Ols.MAX_BLOCKS ) )
    {
      throw new IllegalArgumentException( "Invalid channel group: " + aGroupNr + "!" );
    }

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
   * Returns whether the device we're talking to need a high or low DTR-line to
   * operate correctly.
   * <p>
   * For Arduino devices, the DTR line is used for auto-reset purposes, which
   * should be set to high. No other devices make use of the DTR line (as far as
   * I know).
   * </p>
   * 
   * @see DeviceProfile#isOpenPortDtr()
   */
  public boolean isOpenPortDtr()
  {
    if ( this.deviceProfile == null )
    {
      return false;
    }
    return this.deviceProfile.isOpenPortDtr();
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
   * Returns whether or not the device returns its samples in "reverse" order.
   * 
   * @return <code>true</code> if the samples are returned in reverse order,
   *         <code>false</code> otherwise.
   */
  public boolean isSamplesInReverseOrder()
  {
    if ( this.deviceProfile != null )
    {
      return this.deviceProfile.isSamplesInReverseOrder();
    }
    return false;
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
   * Sets connectionURI to the given value.
   * 
   * @param aConnectionURI
   *          the connectionURI to set.
   */
  public void setConnectionURI( final String aConnectionURI )
  {
    if ( ( aConnectionURI == null ) || aConnectionURI.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "ConnectionURI cannot be null!" );
    }
    this.connectionURI = aConnectionURI;
  }

  /**
   * Set enabled channels.
   * 
   * @param aMask
   *          bit map defining enabled channels
   */
  public void setEnabledChannels( final int aMask )
  {
    this.enabledChannels = ( aMask & 0xFFFFFFFF );

    // determine enabled groups
    int newRLEDataBytes = 0;
    int mask = this.enabledChannels;
    for ( int i = 0; i < this.enabledGroups.length; i++ )
    {
      final boolean groupEnabled = ( mask & 0xff ) != 0x00;

      this.enabledGroups[i] = groupEnabled;
      if ( groupEnabled )
      {
        newRLEDataBytes++;
      }

      mask >>= 8;
    }

    // keep DDR into consideration...
    if ( isDoubleDataRateEnabled() )
    {
      newRLEDataBytes = Math.min( MAX_CHANNEL_GROUPS_DDR, newRLEDataBytes );
    }

    this.rleDataWidth = 8 * newRLEDataBytes;
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
    if ( ( aStage < 0 ) || ( aStage >= TRIGGER_STAGES ) )
    {
      throw new IllegalArgumentException( "Invalid trigger stage: " + aStage + "!" );
    }

    if ( isDoubleDataRateEnabled() )
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
   * Sets the ratio for samples to read before and after started.
   * 
   * @param aRatio
   *          value between 0 and 1; 0 means all before start, 1 all after
   * @throws IllegalArgumentException
   *           in case the given ratio was less than 0.0, or more than 1.0.
   */
  public void setRatio( final double aRatio )
  {
    if ( aRatio < 0.0 )
    {
      throw new IllegalArgumentException( "Ratio cannot be negative!" );
    }
    if ( aRatio > 1.0 )
    {
      throw new IllegalArgumentException( "Ratio cannot be more than one!" );
    }
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
   * @throws IllegalArgumentException
   *           in case the given count was zero.
   */
  public void setSampleCount( final int aCount ) throws IllegalArgumentException
  {
    if ( aCount == 0 )
    {
      throw new IllegalArgumentException( "Sample count cannot be zero!" );
    }
    this.size = aCount & 0xFFFFF;
  }

  /**
   * Set the sampling rate. All rates must be a divisor of 200.000.000. Other
   * rates will be adjusted to a matching divisor.
   * 
   * @param aSampleRate
   *          the sampling rate in Hertz (Hz).
   * @throws IllegalArgumentException
   *           in case the given rate was 0.
   * @see #getDivider()
   * @see #isDoubleDataRateEnabled()
   */
  public void setSampleRate( final int aSampleRate ) throws IllegalArgumentException
  {
    if ( aSampleRate == 0 )
    {
      throw new IllegalArgumentException( "Sample rate cannot be zero!" );
    }

    // Limit the maximum sample rate to 268435455 (0xFFFFFFF)...
    this.sampleRate = aSampleRate & 0xFFFFFFF;
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
    if ( ( aStage < 0 ) || ( aStage >= TRIGGER_STAGES ) )
    {
      throw new IllegalArgumentException( "Invalid trigger stage: " + aStage + "!" );
    }

    if ( isDoubleDataRateEnabled() )
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

  /**
   * Sets the device profile for this configuration.
   * 
   * @param aDeviceProfile
   *          the device profile to set, cannot be <code>null</code>.
   */
  final void setDeviceProfile( final DeviceProfile aDeviceProfile )
  {
    this.deviceProfile = aDeviceProfile;
  }

  /**
   * Returns whether the demultiplexer is supported by the current device type.
   * If the demultiplexer is enabled, the sampling frequency is doubled.
   * 
   * @return <code>true</code> if the demultiplexer is supported (the default),
   *         <code>false</code> otherwise.
   */
  private boolean isDoubleDataRateSupported()
  {
    boolean result = true;
    if ( this.deviceProfile != null )
    {
      result = this.deviceProfile.isDoubleDataRateSupported();
    }

    return result;
  }
}
