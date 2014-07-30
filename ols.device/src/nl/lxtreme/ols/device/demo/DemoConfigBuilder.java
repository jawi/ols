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


import nl.lxtreme.ols.common.*;


/**
 * Builder for creating a DemoConfig
 */
public class DemoConfigBuilder
{
  // VARIABLES

  private int channels;
  private int dataLength;
  private DataGenerator generator;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DemoConfigBuilder} instance.
   */
  public DemoConfigBuilder()
  {
    this.channels = 1;
    this.dataLength = -1;
    this.generator = null;
  }

  // METHODS

  /**
   * @return the device configuration, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case building the configuration failed.
   */
  public DemoConfig build() throws IllegalArgumentException
  {
    if ( this.channels < 1 || this.channels >= OlsConstants.MAX_CHANNELS )
    {
      throw new IllegalArgumentException( "Invalid number of channels!" );
    }
    if ( this.dataLength < 1 )
    {
      throw new IllegalArgumentException( "Invalid data length" );
    }
    if ( this.generator == null )
    {
      throw new IllegalArgumentException( "Invalid data generator!" );
    }
    return new DemoConfig( this.channels, this.dataLength, this.generator );
  }

  /**
   * @param aChannelCount
   * @return this builder.
   */
  public DemoConfigBuilder setChannelCount( int aChannelCount )
  {
    if ( this.channels < 1 || this.channels >= OlsConstants.MAX_CHANNELS )
    {
      throw new IllegalArgumentException( "Invalid number of channels!" );
    }
    this.channels = aChannelCount;
    return this;
  }

  /**
   * @param aLength
   * @return this builder.
   */
  public DemoConfigBuilder setDataLength( int aLength )
  {
    if ( aLength < 1 )
    {
      throw new IllegalArgumentException( "Invalid data length" );
    }
    this.dataLength = aLength;
    return this;
  }

  /**
   * @param aGenerator
   * @return this builder.
   */
  public DemoConfigBuilder setDataGenerator( DataGenerator aGenerator )
  {
    if ( aGenerator == null )
    {
      throw new IllegalArgumentException( "Invalid data generator!" );
    }
    this.generator = aGenerator;
    return this;
  }
}
