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
package nl.lxtreme.ols.device.sump.config;


import static nl.lxtreme.ols.device.sump.SumpConstants.*;
import static nl.lxtreme.ols.device.sump.SumpFlagBits.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.device.sump.*;
import nl.lxtreme.ols.device.sump.profile.*;
import nl.lxtreme.ols.device.sump.profile.DeviceProfile.CaptureClockSource;


/**
 * Builder for creating a {@link SumpConfig} instance.
 */
public final class SumpConfigBuilder
{
  // VARIABLES

  private final List<SumpBasicTriggerBuilder> triggers;
  private int flags;
  private int enabledChannels;
  private int size;
  private double ratio;
  private int sampleRate;
  private String connectionURI;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpConfigBuilder} instance.
   * 
   * @param aProfile
   *          the device profile to create a configuration for, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given profile was <code>null</code>.
   */
  public SumpConfigBuilder()
  {
    this.triggers = new ArrayList<SumpBasicTriggerBuilder>( MAX_SIMPLE_TRIGGER_STAGES );

    setClockSource( CaptureClockSource.INTERNAL );
    this.ratio = 1.0;
    this.size = 512;
  }

  // METHODS

  /**
   * @return a new {@link SumpConfig} instance, never <code>null</code>.
   */
  public SumpConfig build( DeviceProfile aProfile )
  {
    final boolean demuxMode = DEMUX_MODE.isSet( this.flags );

    int channelCount = aProfile.getChannelCount();
    if ( demuxMode )
    {
      channelCount = Math.min( 16, channelCount );
    }

    // TODO support advanced triggers...
    SumpBasicTrigger[] triggerDefs = new SumpBasicTrigger[this.triggers.size()];
    boolean triggerEnabled = triggerDefs.length > 0;

    for ( int i = 0; i < triggerDefs.length; i++ )
    {
      triggerDefs[i] = this.triggers.get( i ).build( aProfile.getTriggerStages(), channelCount, demuxMode );
    }

    double clock = ( demuxMode ? 2.0 : 1.0 ) * aProfile.getDividerClockspeed();
    double ratio = triggerEnabled ? this.ratio : 1.0;

    int readCount = this.size;
    int delayCount = ( int )( readCount * ratio );

    if ( Integer.bitCount( ( ~this.flags >> 2 ) & 0x0F ) == 0 )
    {
      throw new IllegalArgumentException( "No channel (group)s enabled?!" );
    }

    if ( demuxMode )
    {
      // when DDR is selected, the groups selected in the upper two channel
      // groups must be the same as those selected in the lower two groups
      DISABLE_GROUP_3.apply( this.flags, DISABLE_GROUP_1.isSet( this.flags ) );
      DISABLE_GROUP_4.apply( this.flags, DISABLE_GROUP_2.isSet( this.flags ) );

      // if the demux bit is set, the filter flag *must* be cleared...
      apply( NOISE_FILTER, false );
    }

    Map<String, Serializable> config = new HashMap<String, Serializable>();
    config.put( KEY_CONNECTION_URI, this.connectionURI );
    config.put( KEY_GROUP_COUNT, aProfile.getChannelGroupCount() );
    config.put( KEY_ENABLED_CHANNELS, this.enabledChannels );
    config.put( KEY_SAMPLE_RATE, this.sampleRate );
    config.put( KEY_LAST_SAMPLE_SENT_FIRST, aProfile.isLastSampleSentFirst() );
    config.put( KEY_READ_DELAY_COUNT_COMBINED, aProfile.isReadDelayCountCombined() );
    config.put( KEY_DIVIDER, ( int )Math.max( 0.0, ( ( clock / this.sampleRate ) - 1.0 ) ) );
    config.put( KEY_READ_COUNT, readCount );
    config.put( KEY_DELAY_COUNT, delayCount );
    config.put( KEY_FLAGS, this.flags );
    config.put( KEY_TRIGGER_DEFS, triggerDefs );
    return new SumpConfig( config );
  }

  public SumpBasicTriggerBuilder createSumpTrigger()
  {
    return new SumpBasicTriggerBuilder();
  }

  public SumpConfigBuilder add( SumpBasicTriggerBuilder aTriggerBuilder )
  {
    this.triggers.add( aTriggerBuilder );
    return this;
  }

  /**
   * Sets whether or not the alternative numbering scheme is to be used.
   * 
   * @param aEnable
   *          <code>true</code> if the alternative numbering scheme is to be
   *          used, <code>false</code> otherwise.
   */
  public SumpConfigBuilder setAltNumberSchemeEnabled( boolean aEnable )
  {
    apply( SWAP_NUMBER_SCHEME, aEnable );
    return this;
  }

  /**
   * Sets the clock source to use.
   * 
   * @param aSource
   *          the clock source to use, cannot be <code>null</code>.
   */
  public SumpConfigBuilder setClockSource( CaptureClockSource aSource )
  {
    apply( EXTERNAL_CLOCK_SOURCE, aSource.isExternal() );
    apply( INVERT_CLOCK_SOURCE, aSource.isInverted() );
    return this;
  }

  /**
   * Sets connectionURI to the given value.
   * 
   * @param aConnectionURI
   *          the connectionURI to set.
   */
  public SumpConfigBuilder setConnectionURI( String aConnectionURI )
  {
    if ( ( aConnectionURI == null ) || aConnectionURI.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "ConnectionURI cannot be null!" );
    }
    this.connectionURI = aConnectionURI;
    return this;
  }

  /**
   * Sets whether or not double-data rate (demux) mode is to be enabled.
   * 
   * @param aEnable
   *          <code>true</code> to enable demux (DDR) mode, <code>false</code>
   *          to disable it.
   * @return this builder.
   */
  public SumpConfigBuilder setDoubleDataRateMode( boolean aEnable )
  {
    apply( DEMUX_MODE, aEnable );
    return this;
  }

  /**
   * Set enabled channels.
   * 
   * @param aMask
   *          bit map defining enabled channels
   */
  public SumpConfigBuilder setEnabledChannels( int aMask )
  {
    this.enabledChannels = aMask & 0xFFFFFFFF;

    // determine enabled/disabled groups
    apply( DISABLE_GROUP_1, ( this.enabledChannels & 0x000000FF ) == 0 );
    apply( DISABLE_GROUP_2, ( this.enabledChannels & 0x0000FF00 ) == 0 );
    apply( DISABLE_GROUP_3, ( this.enabledChannels & 0x00FF0000 ) == 0 );
    apply( DISABLE_GROUP_4, ( this.enabledChannels & 0xFF000000 ) == 0 );

    return this;
  }

  public SumpConfigBuilder setEnableGroup1( boolean aEnable )
  {
    enableGroup( DISABLE_GROUP_1.getMask(), 0xFF, aEnable );
    return this;
  }

  public SumpConfigBuilder setEnableGroup2( boolean aEnable )
  {
    enableGroup( DISABLE_GROUP_2.getMask(), 0xFF00, aEnable );
    return this;
  }

  public SumpConfigBuilder setEnableGroup3( boolean aEnable )
  {
    enableGroup( DISABLE_GROUP_3.getMask(), 0xFF0000, aEnable );
    return this;
  }

  public SumpConfigBuilder setEnableGroup4( boolean aEnable )
  {
    enableGroup( DISABLE_GROUP_4.getMask(), 0xFF000000, aEnable );
    return this;
  }

  /**
   * Sets whether or not the noise filter is enabled.
   * 
   * @param aEnable
   *          <code>true</code> to enable the noise filter, <code>false</code>
   *          to disable it.
   */
  public SumpConfigBuilder setFilterEnabled( boolean aEnable )
  {
    apply( NOISE_FILTER, aEnable );
    return this;
  }

  /**
   * Sets the ratio for samples to read before and after started.
   * 
   * @param aRatio
   *          value between 0 and 1; 0 means all before start, 1 all after
   * @throws IllegalArgumentException
   *           in case the given ratio was less than 0.0, or more than 1.0.
   */
  public SumpConfigBuilder setRatio( double aRatio )
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
    return this;
  }

  /**
   * Sets whether or not to enable the run length encoding.
   * 
   * @param enable
   *          <code>true</code> to enable the RLE, <code>false</code> to disable
   *          it.
   */
  public SumpConfigBuilder setRleEnabled( boolean aEnable, int aMode )
  {
    apply( ENABLE_RLE_MODE, aEnable );
    switch ( aMode )
    {
      case 1:
        apply( RLE_MODE_1, aEnable );
        break;
      case 2:
        apply( RLE_MODE_2, aEnable );
        break;
      case 3:
        apply( RLE_MODE_3, aEnable );
        break;
    }
    return this;
  }

  /**
   * Sets the number of samples to obtain when started.
   * 
   * @param aCount
   *          the number of samples to take, must be between 4 and 256*1024.
   * @throws IllegalArgumentException
   *           in case the given count was zero.
   */
  public SumpConfigBuilder setSampleCount( final int aCount ) throws IllegalArgumentException
  {
    if ( aCount == 0 )
    {
      throw new IllegalArgumentException( "Sample count cannot be zero!" );
    }
    this.size = aCount;
    return this;
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
  public SumpConfigBuilder setSampleRate( final int aSampleRate ) throws IllegalArgumentException
  {
    if ( aSampleRate == 0 )
    {
      throw new IllegalArgumentException( "Sample rate cannot be zero!" );
    }
    // Limit the maximum sample rate to 268435455 (~ 268MHz) (0xFFFFFFF)...
    this.sampleRate = aSampleRate & 0x0FFFFFFF;
    return this;
  }

  /**
   * Sets whether or not to enable the test mode.
   * 
   * @param aEnable
   *          <code>true</code> to enable the test mode, <code>false</code> to
   *          disable it.
   */
  public SumpConfigBuilder setTestModeEnabled( final boolean aEnable )
  {
    apply( EXTERNAL_TEST_MODE, aEnable );
    return this;
  }

  private void apply( SumpFlagBits flag, boolean aEnable )
  {
    this.flags = flag.apply( this.flags, aEnable );
  }

  private void enableGroup( int aGroupMask, int aMask, boolean aEnable )
  {
    if ( aEnable )
    {
      this.enabledChannels |= aMask;
      this.flags &= ~aGroupMask;
    }
    else
    {
      this.enabledChannels &= ~aMask;
      this.flags |= aGroupMask;
    }
  }
}
