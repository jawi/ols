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
package nl.lxtreme.ols.device.demo;


import static nl.lxtreme.ols.device.demo.DemoConstants.*;

import java.util.*;

import nl.lxtreme.ols.device.api.*;


/**
 * Represents the configuration for the demo device.
 */
public class DemoConfig implements DeviceConfiguration
{
  // VARIABLES

  private final int channelCount;
  private final int sampleCount;
  private final DataGenerator generator;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DemoConfig} instance from a serialized map.
   */
  public DemoConfig( Map<String, String> aConfig )
  {
    if ( aConfig == null )
    {
      throw new IllegalArgumentException( "Configuration cannot be null!" );
    }

    try
    {
      String strChan = aConfig.get( KEY_CHANNEL_COUNT );
      this.channelCount = Integer.parseInt( strChan );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid number of channels!" );
    }

    try
    {
      String strDataLen = aConfig.get( KEY_SAMPLE_COUNT );
      this.sampleCount = Integer.parseInt( strDataLen );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid data length" );
    }

    String strGen = aConfig.get( KEY_GENERATOR_NAME );
    this.generator = DemoDevice.getGeneratorByName( strGen );
  }

  /**
   * Creates a new {@link DemoConfig} instance.
   * 
   * @param aChannelCount
   *          the number of channels;
   * @param aSampleCount
   *          the length of the data to generate;
   * @param aGenerator
   *          the data generator to use.
   */
  DemoConfig( int aChannelCount, int aSampleCount, DataGenerator aGenerator )
  {
    this.channelCount = aChannelCount;
    this.sampleCount = aSampleCount;
    this.generator = aGenerator;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, String> asMap()
  {
    Map<String, String> result = new HashMap<String, String>();
    result.put( KEY_CHANNEL_COUNT, Integer.toString( this.channelCount ) );
    result.put( KEY_SAMPLE_COUNT, Integer.toString( this.sampleCount ) );
    result.put( KEY_GENERATOR_NAME, this.generator.getName() );
    return result;
  }

  /**
   * Returns the number of channels in the generated data.
   * 
   * @return the number of channels, &gt; 0.
   */
  public int getChannelCount()
  {
    return this.channelCount;
  }

  /**
   * Returns the data generator to use.
   * 
   * @return the data generator, never <code>null</code>.
   */
  public DataGenerator getGenerator()
  {
    return this.generator;
  }

  /**
   * Returns the number of samples to generate.
   * 
   * @return the sample count, &gt; 0.
   */
  public int getSampleCount()
  {
    return this.sampleCount;
  }
}
