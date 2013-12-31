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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump;


import static nl.lxtreme.ols.device.sump.SumpConstants.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.device.sump.profile.*;
import nl.lxtreme.ols.device.sump.profile.DeviceProfile.*;
import nl.lxtreme.ols.device.sump.protocol.*;


/**
 * Builder for creating a {@link SumpConfig} instance.
 */
public final class SumpConfigBuilder
{
  // CONSTANTS

  // trigger will start capture when fired
  public final static int TRIGGER_CAPTURE = 0x08000000;

  // mask for delay value
  private final static int TRIGGER_DELAYMASK = 0x0000ffff;
  // mask for level value
  private final static int TRIGGER_LEVELMASK = 0x00030000;
  // mask for channel value
  private final static int TRIGGER_CHANNELMASK = 0x01f00000;
  // trigger operates in serial mode
  private final static int TRIGGER_SERIAL = 0x04000000;

  // VARIABLES

  private final DeviceProfile profile;

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
  private int sampleRate;
  private String connectionURI;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpConfigBuilder} instance.
   */
  public SumpConfigBuilder( DeviceProfile aProfile )
  {
    this.profile = aProfile;

    this.triggerMask = new int[4];
    this.triggerValue = new int[4];
    this.triggerConfig = new int[4];
    for ( int i = 0; i < MAX_COMPLEX_TRIGGER_STAGES; i++ )
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
  static final int determineRleMode()
  {
    return Integer.getInteger( "nl.lxtreme.ols.rle.mode", 1 ).intValue();
  }

  /**
   * @return a new {@link SumpConfig} instance, never <code>null</code>.
   */
  public SumpConfig build()
  {
    double clock = ( isDoubleDataRateEnabled() ? 2.0 : 1.0 ) * getDividerClockspeed();

    int readCount = this.size;
    int delayCount = this.triggerEnabled ? ( int )( readCount * this.ratio ) : readCount;

    int flags = 0;
    if ( CaptureClockSource.INTERNAL != this.clockSource )
    {
      flags |= FLAG_EXTERNAL;
      if ( CaptureClockSource.EXTERNAL_FALLING == this.clockSource )
      {
        flags |= FLAG_INVERTED;
      }
    }

    // determine which channel groups are to be disabled...
    int enabledChannelGroups = 0;
    for ( int i = 0; i < getGroupCount(); i++ )
    {
      if ( this.enabledGroups[i] )
      {
        enabledChannelGroups |= ( 1 << i );
      }
    }

    if ( enabledChannelGroups == 0 )
    {
      throw new IllegalArgumentException( "No channels enabled?!" );
    }

    if ( isDoubleDataRateEnabled() )
    {
      // when DDR is selected, the groups selected in the upper two channel
      // groups must be the same as those selected in the lower two groups
      enabledChannelGroups |= ( ( enabledChannelGroups & 0x03 ) << 2 ) & 0x0c;

      flags |= FLAG_DEMUX;
      // if the demux bit is set, the filter flag *must* be cleared...
      flags &= ~FLAG_FILTER;
    }

    flags |= ~( enabledChannelGroups << 2 ) & 0x3c;

    if ( this.filterEnabled && this.profile.isNoiseFilterSupported() && !isDoubleDataRateEnabled() )
    {
      flags |= FLAG_FILTER;
      // if the filter bit is set, the demux flag *must* be cleared...
      flags &= ~FLAG_DEMUX;
    }

    if ( this.profile.isRleSupported() && this.rleEnabled )
    {
      flags |= FLAG_RLE;

      // Ian 'dogsbody''s Verilog understands four different RLE-modes...
      switch ( determineRleMode() )
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

    if ( this.altNumberSchemeEnabled )
    {
      flags |= FLAG_NUMBER_SCHEME;
    }
    if ( this.testModeEnabled )
    {
      flags |= FLAG_EXTERNAL_TEST_MODE;
    }

    int channelCount = this.profile.getChannelCount();
    if ( isDoubleDataRateEnabled() )
    {
      channelCount = Math.min( 16, channelCount );
    }

    SumpConfig result = new SumpConfig();
    result.put( KEY_CONNECTION_URI, this.connectionURI );
    result.put( KEY_GROUP_COUNT, getGroupCount() );
    result.put( KEY_CHANNEL_COUNT, channelCount );
    result.put( KEY_ENABLED_CHANNELS, this.enabledChannels );
    result.put( KEY_SAMPLE_RATE, this.sampleRate );
    result.put( KEY_LAST_SAMPLE_SENT_FIRST, this.profile.isLastSampleSentFirst() );
    result.put( KEY_READ_DELAY_COUNT_COMBINED, this.profile.isReadDelayCountCombined() );
    result.put( KEY_DIVIDER, ( int )Math.max( 0.0, ( ( clock / this.sampleRate ) - 1.0 ) ) );
    result.put( KEY_READ_COUNT, readCount );
    result.put( KEY_DELAY_COUNT, delayCount );
    result.put( KEY_FLAGS, flags );
    result.put( KEY_TRIGGER_ENABLED, this.triggerEnabled );
    result.put( KEY_TRIGGER_STAGES, this.profile.getTriggerStages() );
    result.put( KEY_TRIGGER_CONFIG, this.triggerConfig );
    result.put( KEY_TRIGGER_MASK, this.triggerMask );
    result.put( KEY_TRIGGER_VALUE, this.triggerValue );
    return result;
  }

  /**
   * Returns the number of <em>available</em> channels in current configuration.
   * 
   * @return the total number of available channels, e.g., 8, 16, 24 or 32.
   */
  private int getChannelCount()
  {
    int channels = 32;
    if ( isDoubleDataRateEnabled() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      channels = 16;
    }

    if ( this.profile != null )
    {
      channels = Math.min( channels, this.profile.getChannelCount() );
    }

    return channels;
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
  private int getGroupCount()
  {
    int cnt = Math.max( MIN_CHANNEL_GROUPS, getChannelCount() / OlsConstants.CHANNELS_PER_BLOCK );
    if ( isDoubleDataRateEnabled() )
    {
      // In case the demux is enabled, only a maximum of two channel groups is
      // allowed...
      cnt = Math.min( MAX_CHANNEL_GROUPS_DDR, cnt );
    }

    return cnt;
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
    int mask = this.enabledChannels;
    for ( int i = 0; i < this.enabledGroups.length; i++ )
    {
      final boolean groupEnabled = ( mask & 0xff ) != 0x00;

      this.enabledGroups[i] = groupEnabled;

      mask >>= 8;
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
    if ( ( aStage < 0 ) || ( aStage >= MAX_COMPLEX_TRIGGER_STAGES ) )
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
    this.size = aCount;
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
    if ( ( aStage < 0 ) || ( aStage >= MAX_COMPLEX_TRIGGER_STAGES ) )
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
   * Returns whether the demultiplexer is used. If the demultiplexer is enabled,
   * the sampling frequency is doubled to 200MHz.
   * 
   * @return <code>true</code> if the demultiplexer is enabled,
   *         <code>false</code> otherwise.
   * @see #setSampleRate(int)
   */
  private boolean isDoubleDataRateEnabled()
  {
    if ( this.clockSource != CaptureClockSource.INTERNAL || !isDoubleDataRateSupported() )
    {
      return false;
    }

    return ( this.sampleRate > getDividerClockspeed() );
  }

  /**
   * Returns the clockspeed used in the divider calculation.
   * 
   * @return a clockspeed, in Hertz (Hz), defaults to 100MHz.
   */
  private int getDividerClockspeed()
  {
    int result = SumpProtocolConstants.CLOCK;
    if ( this.profile != null )
    {
      result = this.profile.getDividerClockspeed();
    }
    return result;
  }

  /**
   * Returns whether the demultiplexer is supported by the current device type.
   * If the demultiplexer is enabled, the sampling frequency is doubled.
   * 
   * @return <code>true</code> if the demultiplexer is supported,
   *         <code>false</code> (the default) otherwise.
   */
  private boolean isDoubleDataRateSupported()
  {
    boolean result = false;
    if ( this.profile != null )
    {
      result = this.profile.isDoubleDataRateSupported();
    }
    return result;
  }
}
