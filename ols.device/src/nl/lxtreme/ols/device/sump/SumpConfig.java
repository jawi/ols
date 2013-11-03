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
package nl.lxtreme.ols.device.sump;


import static nl.lxtreme.ols.device.sump.SumpConstants.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.common.*;


/**
 * Provides the configuration options for the LogicSniffer device.
 */
public final class SumpConfig extends HashMap<String, Serializable>
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SumpConfig} instance with all default settings.
   */
  public SumpConfig()
  {
    super();
  }

  public SumpConfig( Map<String, ? extends Serializable> aConfig )
  {
    super( aConfig );
  }

  // METHODS

  public int getChannelCount()
  {
    return ( Integer )get( KEY_CHANNEL_COUNT );
  }

  public String getConnectionURI()
  {
    return ( String )get( KEY_CONNECTION_URI );
  }

  /**
   * @return the actual delay count value as should be sent to the device, > 0.
   */
  public int getDelayCount()
  {
    int result = ( Integer )get( KEY_DELAY_COUNT );
    if ( isDoubleDataRateEnabled() )
    {
      result >>= 3;
    }
    else
    {
      result >>= 2;
    }
    return result - 1;
  }

  public int getDivider()
  {
    return ( Integer )get( KEY_DIVIDER );
  }

  public int getEnabledChannelCount()
  {
    return Math.min( getChannelCount(), OlsConstants.CHANNELS_PER_BLOCK * getEnabledGroupCount() );
  }

  public int getEnabledChannelMask()
  {
    return ( Integer )get( KEY_ENABLED_CHANNELS );
  }

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

  public int getFlags()
  {
    return ( Integer )get( KEY_FLAGS );
  }

  public int getGroupCount()
  {
    return ( Integer )get( KEY_GROUP_COUNT );
  }

  /**
   * @return the actual "read count" value as should be sent to the device, > 0.
   */
  public int getReadCount()
  {
    int result = ( Integer )get( KEY_READ_COUNT );
    if ( isDoubleDataRateEnabled() )
    {
      result >>= 3;
    }
    else
    {
      result >>= 2;
    }
    return result - 1;
  }

  public int getTriggerPosition()
  {
    int readCount = getReadCount();
    int delayCount = getDelayCount();

    if ( isDoubleDataRateEnabled() )
    {
      return ( readCount << 3 ) - ( delayCount << 3 );
    }

    return ( readCount << 2 ) - ( delayCount << 2 );
  }

  /**
   * Returns the number of samples to be taken in current configuration.
   * 
   * @return number of samples, >= 0.
   */
  public int getSampleCount()
  {
    int samples = ( Integer )get( KEY_READ_COUNT );
    if ( isDoubleDataRateEnabled() )
    {
      // When the multiplexer is turned on, the upper two channel blocks are
      // disabled, leaving only 16 channels for capturing...
      samples = ( samples >> 1 ) & 0xfffffff8;
    }
    else
    {
      samples &= 0xfffffffc;
    }
    return samples;
  }

  public int getCombinedReadDelayCount()
  {
    int readCount = ( Integer )get( KEY_READ_COUNT );
    int delayCount = ( Integer )get( KEY_DELAY_COUNT );

    int maxSize = 0x3fffc;
    if ( isDoubleDataRateEnabled() )
    {
      // 0x7fff8 = 511Kb = the maximum size supported by the original SUMP
      // device when using the demultiplexer...
      maxSize = 0x7fff8;
      return ( ( delayCount & maxSize ) << 13 ) | ( ( ( readCount & maxSize ) >> 3 ) - 1 );
    }
    // 0x3fffc = 255Kb = the maximum size supported by the original SUMP
    // device...
    return ( ( delayCount & maxSize ) << 14 ) | ( ( ( readCount & maxSize ) >> 2 ) - 1 );
  }

  public int getSampleRate()
  {
    return ( Integer )get( KEY_SAMPLE_RATE );
  }

  public int getTriggerConfig( int aStage )
  {
    int[] configs = ( int[] )get( KEY_TRIGGER_CONFIG );
    return configs[aStage];
  }

  public int getTriggerMask( int aStage )
  {
    int[] masks = ( int[] )get( KEY_TRIGGER_MASK );
    return masks[aStage];
  }

  public int getTriggerStageCount()
  {
    return ( Integer )get( KEY_TRIGGER_STAGES );
  }

  public int getTriggerValue( int aStage )
  {
    int[] values = ( int[] )get( KEY_TRIGGER_VALUE );
    return values[aStage];
  }

  public boolean isDoubleDataRateEnabled()
  {
    return ( getFlags() & 0x01 ) != 0;
  }

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

  public boolean isReadDelayCountValueCombined()
  {
    return ( Boolean )get( KEY_READ_DELAY_COUNT_COMBINED );
  }

  public boolean isRleEnabled()
  {
    return ( getFlags() & 0x100 ) != 0;
  }

  public boolean isSamplesInReverseOrder()
  {
    return ( Boolean )get( KEY_SAMPLES_READ_BACKWARD );
  }

  public boolean isTriggerEnabled()
  {
    return ( Boolean )get( KEY_TRIGGER_ENABLED );
  }

  public boolean isValid()
  {
    return true; // XXX
  }
}
