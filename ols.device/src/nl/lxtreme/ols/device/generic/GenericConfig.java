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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.generic;


import static nl.lxtreme.ols.device.generic.GenericConstants.*;

import java.util.*;

import nl.lxtreme.ols.device.api.*;


/**
 * Represents the configuration for the {@link GenericDevice}.
 */
public class GenericConfig implements DeviceConfiguration
{
  // VARIABLES

  private final String devicePath;
  private final int sampleRate;
  private final int sampleCount;
  private final int sampleWidth;
  private final int channelCount;

  // CONSTRUCTORS

  /**
   * Creates a new {@link GenericConfig} instance from a serialized map.
   */
  public GenericConfig( Map<String, String> aConfig )
  {
    if ( aConfig == null )
    {
      throw new IllegalArgumentException( "Configuration cannot be null!" );
    }

    this.devicePath = aConfig.get( KEY_DEVICE_PATH );

    try
    {
      String sr = aConfig.get( KEY_SAMPLE_RATE );
      this.sampleRate = Integer.parseInt( sr );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid sample rate!" );
    }

    try
    {
      String sc = aConfig.get( KEY_SAMPLE_COUNT );
      this.sampleCount = Integer.parseInt( sc );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid sample count!" );
    }

    try
    {
      String sw = aConfig.get( KEY_SAMPLE_WIDTH );
      this.sampleWidth = Integer.parseInt( sw );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid sample width!" );
    }

    try
    {
      String cc = aConfig.get( KEY_CHANNEL_COUNT );
      this.channelCount = Integer.parseInt( cc );
    }
    catch ( NumberFormatException exception )
    {
      throw new IllegalArgumentException( "Invalid channel count!" );
    }
  }

  /**
   * Creates a new {@link GenericConfig} instance.
   */
  GenericConfig( String aDevicePath, int aSampleRate, int aSampleCount, int aSampleWidth, int aChannelCount )
  {
    this.devicePath = aDevicePath;
    this.sampleRate = aSampleRate;
    this.sampleCount = aSampleCount;
    this.sampleWidth = aSampleWidth;
    this.channelCount = aChannelCount;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, String> asMap()
  {
    Map<String, String> result = new HashMap<String, String>();
    result.put( KEY_DEVICE_PATH, this.devicePath );
    result.put( KEY_SAMPLE_COUNT, Integer.toString( this.sampleCount ) );
    result.put( KEY_SAMPLE_RATE, Integer.toString( this.sampleRate ) );
    result.put( KEY_SAMPLE_WIDTH, Integer.toString( this.sampleWidth ) );
    result.put( KEY_CHANNEL_COUNT, Integer.toString( this.channelCount ) );
    return result;
  }

  /**
   * Returns the current value of channelCount.
   * 
   * @return the channelCount
   */
  public int getChannelCount()
  {
    return this.channelCount;
  }

  /**
   * Returns the current value of devicePath.
   * 
   * @return the devicePath
   */
  public String getDevicePath()
  {
    return this.devicePath;
  }

  /**
   * Returns the current value of sampleCount.
   * 
   * @return the sampleCount
   */
  public int getSampleCount()
  {
    return this.sampleCount;
  }

  /**
   * Returns the current value of sampleRate.
   * 
   * @return the sampleRate
   */
  public int getSampleRate()
  {
    return this.sampleRate;
  }

  /**
   * Returns the current value of sampleWidth.
   * 
   * @return the sampleWidth
   */
  public int getSampleWidth()
  {
    return this.sampleWidth;
  }
}
