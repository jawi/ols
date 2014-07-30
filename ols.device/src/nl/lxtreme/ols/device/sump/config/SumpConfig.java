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
package nl.lxtreme.ols.device.sump.config;


import static nl.lxtreme.ols.device.sump.SumpConstants.*;
import static nl.lxtreme.ols.device.sump.SumpFlagBits.*;

import java.util.*;

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.device.sump.*;
import nl.lxtreme.ols.device.sump.profile.*;


/**
 * Provides the configuration options for the LogicSniffer device.
 */
public final class SumpConfig implements DeviceConfiguration
{
  // VARIABLES

  private final String connectionURI;
  private final int groupCount;
  private final int enabledChannelMask;
  private final int sampleRate;
  private final boolean lastSampleSentFirst;
  private final boolean combinedReadDelayCount;
  private final int readCount;
  private final int delayCount;
  private final int divider;
  private final int flags;
  private final SumpTrigger[] triggers;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpConfig} instance from a serialized map.
   */
  public SumpConfig( Map<String, String> aConfig )
  {
    if ( aConfig == null )
    {
      throw new IllegalArgumentException( "Configuration cannot be null!" );
    }

    this.connectionURI = aConfig.get( KEY_CONNECTION_URI );
    this.groupCount = Integer.parseInt( aConfig.get( KEY_GROUP_COUNT ) );
    this.enabledChannelMask = Integer.parseInt( aConfig.get( KEY_ENABLED_CHANNELS ) );
    this.sampleRate = Integer.parseInt( aConfig.get( KEY_SAMPLE_RATE ) );
    this.lastSampleSentFirst = Boolean.parseBoolean( aConfig.get( KEY_LAST_SAMPLE_SENT_FIRST ) );
    this.combinedReadDelayCount = Boolean.parseBoolean( aConfig.get( KEY_READ_DELAY_COUNT_COMBINED ) );
    this.readCount = Integer.parseInt( aConfig.get( KEY_READ_COUNT ) );
    this.delayCount = Integer.parseInt( aConfig.get( KEY_DELAY_COUNT ) );
    this.divider = Integer.parseInt( aConfig.get( KEY_DIVIDER ) );
    this.flags = Integer.parseInt( aConfig.get( KEY_FLAGS ) );
    this.triggers = null; // TODO

  }

  /**
   * Creates a new {@link SumpConfig} instance.
   */
  SumpConfig( String aConnectionURI, DeviceProfile aProfile, int aEnabledChannelMask, int aSampleRate, int aReadCount,
      int aDelayCount, int aDivider, int aFlags, SumpTrigger[] aTriggers )
  {
    this.connectionURI = aConnectionURI;
    this.groupCount = aProfile.getChannelGroupCount();
    this.enabledChannelMask = aEnabledChannelMask;
    this.sampleRate = aSampleRate;
    this.lastSampleSentFirst = aProfile.isLastSampleSentFirst();
    this.combinedReadDelayCount = aProfile.isReadDelayCountCombined();
    this.readCount = aReadCount;
    this.delayCount = aDelayCount;
    this.divider = aDivider;
    this.flags = aFlags;
    this.triggers = aTriggers;
  }

  // METHODS

  /**
   * Returns this configuration as {@link Map}.
   * 
   * @return the configuration settings, as unmodifiable {@link Map}, never
   *         <code>null</code>.
   */
  public Map<String, String> asMap()
  {
    Map<String, String> result = new HashMap<String, String>();
    result.put( KEY_TRIGGER_DEFS, "" ); // TODO
    result.put( KEY_READ_DELAY_COUNT_COMBINED, Boolean.toString( combinedReadDelayCount ) );
    result.put( KEY_CONNECTION_URI, this.connectionURI );
    result.put( KEY_DELAY_COUNT, Integer.toString( this.delayCount ) );
    result.put( KEY_DIVIDER, Integer.toString( this.divider ) );
    result.put( KEY_ENABLED_CHANNELS, Integer.toString( this.enabledChannelMask ) );
    result.put( KEY_FLAGS, Integer.toString( this.flags ) );
    result.put( KEY_GROUP_COUNT, Integer.toString( this.groupCount ) );
    result.put( KEY_LAST_SAMPLE_SENT_FIRST, Boolean.toString( this.lastSampleSentFirst ) );
    result.put( KEY_READ_COUNT, Integer.toString( this.readCount ) );
    result.put( KEY_SAMPLE_RATE, Integer.toString( this.sampleRate ) );
    return result;
  }

  /**
   * @return the advanced trigger definitions, as array.
   */
  public SumpAdvancedTrigger[] getAdvancedTriggerDefinitions()
  {
    return ( SumpAdvancedTrigger[] )this.triggers;
  }

  /**
   * @return the basic trigger definitions, as array.
   */
  public SumpBasicTrigger[] getBasicTriggerDefinitions()
  {
    return ( SumpBasicTrigger[] )this.triggers;
  }

  /**
   * @return the combined value of the read and delay counters, as used in the
   *         original SUMP protocol.
   */
  public int getCombinedReadDelayCount()
  {
    int maxSize = 0x3fffc;
    if ( isDoubleDataRateEnabled() )
    {
      // 0x7fff8 = 511Kb = the maximum size supported by the original SUMP
      // device when using the demultiplexer...
      maxSize = 0x7fff8;
      return ( ( ( delayCount - 8 ) & maxSize ) << 13 ) | ( ( ( readCount & maxSize ) >> 3 ) - 1 );
    }
    // 0x3fffc = 255Kb = the maximum size supported by the original SUMP
    // device...
    return ( ( ( delayCount - 4 ) & maxSize ) << 14 ) | ( ( ( readCount & maxSize ) >> 2 ) - 1 );
  }

  /**
   * @return the connection URI, as string, never <code>null</code>.
   */
  public String getConnectionURI()
  {
    return this.connectionURI;
  }

  /**
   * @return the actual delay count value as should be sent to the device, > 0.
   */
  public int getDelayCount()
  {
    int delayCount = this.delayCount;
    if ( isDoubleDataRateEnabled() )
    {
      return ( ( delayCount - 8 ) & 0x7fffff8 ) >> 3;
    }
    return ( ( delayCount - 4 ) & 0x3ffffffc ) >> 2;
  }

  /**
   * @return the divider value to set, which represents the sample rate used by
   *         the SUMP device.
   */
  public int getDivider()
  {
    return this.divider;
  }

  /**
   * @return the number of *enabled* channels, based on the enabled channel
   *         groups, &gt; 0 && &lt= 32.
   */
  public int getEnabledChannelCount()
  {
    return Integer.bitCount( getEnabledChannelMask() );
  }

  /**
   * @return the 32-bit bit mask denoting which channels are enabled.
   */
  public int getEnabledChannelMask()
  {
    return this.enabledChannelMask;
  }

  /**
   * @return the number of enabled channel groups, &gt; 0 && &lt;= 3.
   */
  public int getEnabledGroupCount()
  {
    int enabledGroups = ( ~getFlags() >> 2 );
    if ( isDoubleDataRateEnabled() )
    {
      enabledGroups &= 0x3;
    }
    else
    {
      enabledGroups &= 0xF;
    }
    return Integer.bitCount( enabledGroups );
  }

  /**
   * @return the flags value representing the various flags for the SUMP device.
   */
  public int getFlags()
  {
    return this.flags;
  }

  /**
   * @return the maximum number of groups supported by the SUMP device, &gt; 0
   *         && &lt;= 3.
   */
  public int getGroupCount()
  {
    return this.groupCount;
  }

  /**
   * @return the actual "read count" value as should be sent to the device, > 0.
   */
  public int getReadCount()
  {
    int readCount = this.readCount;
    if ( isDoubleDataRateEnabled() )
    {
      return ( readCount >> 3 ) - 1;
    }

    return ( readCount >> 2 ) - 1;
  }

  /**
   * Returns the number of samples to be taken in current configuration.
   * 
   * @return number of samples, >= 0.
   */
  public int getSampleCount()
  {
    int samples = this.readCount;
    if ( isDoubleDataRateEnabled() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      samples &= 0xfffffff8;
    }
    else
    {
      samples &= 0xfffffffc;
    }
    return samples;
  }

  /**
   * @return the sample rate to use, in Hertz.
   */
  public int getSampleRate()
  {
    return this.sampleRate;
  }

  /**
   * @return the trigger position, if triggers are enabled, or
   *         {@link OlsConstants#NOT_AVAILABLE} if triggers are disabled.
   */
  public int getTriggerPosition()
  {
    if ( this.triggers == null || this.triggers.length < 1 )
    {
      // Not available...
      return OlsConstants.NOT_AVAILABLE;
    }

    // Get the "raw" values...
    boolean ddr = isDoubleDataRateEnabled();

    // pure magic taken from the original LA sources...
    return readCount - delayCount - 3 - ( 4 / ( divider + 1 ) ) - ( ddr ? 5 : 0 );
  }

  /**
   * @return <code>true</code> if advanced triggers are to be used,
   *         <code>false</code> otherwise.
   */
  public boolean isAdvancedTriggerEnabled()
  {
    return ( this.triggers instanceof SumpAdvancedTrigger[] );
  }

  /**
   * @return <code>true</code> if basic triggers are to be used,
   *         <code>false</code> otherwise.
   */
  public boolean isBasicTriggerEnabled()
  {
    return ( this.triggers instanceof SumpBasicTrigger[] );
  }

  /**
   * @return <code>true</code> if the DDR/demux mode is enabled,
   *         <code>false</code> otherwise.
   */
  public boolean isDoubleDataRateEnabled()
  {
    return DEMUX_MODE.isSet( getFlags() );
  }

  /**
   * @return <code>true</code> if a flush of the input stream upon closing of
   *         the device is needed, <code>false</code> otherwise.
   */
  public boolean isFlushOnCloseNeeded()
  {
    // On the Pipistrello, this can take a *long* time, which is not aiding the
    // user experience...
    return getSampleCount() < 256 * 1024;
  }

  /**
   * @param aGroupIdx
   *          the group index, &gt;= 0 && &lt;= 3.
   * @return <code>true</code> if the group is enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isGroupEnabled( int aGroupIdx )
  {
    int enabledGroups = ( ~getFlags() >> 2 );
    if ( isDoubleDataRateEnabled() )
    {
      enabledGroups &= 0x3;
    }
    else
    {
      enabledGroups &= 0xF;
    }
    return ( enabledGroups & ( 1 << aGroupIdx ) ) != 0;
  }

  /**
   * @return <code>true</code> if the <b>last</b> sample is sent first,
   *         <code>false</code> if the <b>first</b> sample is sent first.
   */
  public boolean isLastSampleSentFirst()
  {
    return this.lastSampleSentFirst;
  }

  /**
   * @return <code>true</code> if the read and delay counter values are to be
   *         combined to a single value before being send to the SUMP device, or
   *         <code>false</code> if they should be send as individual values.
   */
  public boolean isReadDelayCountValueCombined()
  {
    return this.combinedReadDelayCount;
  }

  /**
   * @return <code>true</code> if RLE-compression is enabled, <code>false</code>
   *         if it is disabled.
   */
  public boolean isRleEnabled()
  {
    return ENABLE_RLE_MODE.isSet( getFlags() );
  }
}
