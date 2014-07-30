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


import nl.lxtreme.ols.common.*;


/**
 * Builder for {@link GenericConfig}s.
 */
public class GenericConfigBuilder
{
  // VARIABLES

  private String devicePath;
  private int sampleRate;
  private int sampleCount;
  private int sampleWidth;
  private int channelCount;

  // CONSTRUCTORS

  /**
   * Creates a new {@link GenericConfigBuilder} instance.
   */
  public GenericConfigBuilder()
  {
    this.devicePath = "/dev/random";
    this.sampleRate = 1;
    this.sampleCount = 10;
    this.sampleWidth = 1;
    this.channelCount = 8;
  }

  // METHODS

  /**
   * @return a new {@link GenericConfig} instance, never <code>null</code>.
   */
  public GenericConfig build()
  {
    if ( this.devicePath == null || "".equals( this.devicePath.trim() ) )
    {
      throw new IllegalArgumentException( "DevicePath cannot be null or empty!" );
    }
    if ( this.sampleRate < 1 )
    {
      throw new IllegalArgumentException( "Invalid sample rate!" );
    }
    if ( this.sampleCount < 1 )
    {
      throw new IllegalArgumentException( "Invalid data length" );
    }
    if ( this.sampleWidth < 1 )
    {
      throw new IllegalArgumentException( "Invalid sample width!" );
    }
    if ( this.channelCount < 1 || this.channelCount >= OlsConstants.MAX_CHANNELS )
    {
      throw new IllegalArgumentException( "Invalid number of channels!" );
    }
    return new GenericConfig( this.devicePath, this.sampleRate, this.sampleCount, this.sampleWidth, this.channelCount );
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

  /**
   * @param aChannelCount
   * @return this builder.
   */
  public GenericConfigBuilder setChannelCount( int aChannelCount )
  {
    if ( aChannelCount < 1 || aChannelCount >= OlsConstants.MAX_CHANNELS )
    {
      throw new IllegalArgumentException( "Invalid number of channels!" );
    }
    this.channelCount = aChannelCount;
    return this;
  }

  /**
   * @param aChannelCount
   * @return this builder.
   */
  public GenericConfigBuilder setChannelCount( String aChannelCount )
  {
    try
    {
      return setChannelCount( Integer.parseInt( aChannelCount ) );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid number of channels!" );
    }
  }

  /**
   * @param aDevicePath
   * @return this builder
   */
  public GenericConfigBuilder setDevicePath( String aDevicePath )
  {
    if ( aDevicePath == null || "".equals( aDevicePath.trim() ) )
    {
      throw new IllegalArgumentException( "DevicePath cannot be null or empty!" );
    }
    this.devicePath = aDevicePath;
    return this;
  }

  /**
   * @param aSampleCount
   * @return this builder.
   */
  public GenericConfigBuilder setSampleCount( int aSampleCount )
  {
    if ( aSampleCount < 1 )
    {
      throw new IllegalArgumentException( "Invalid sample count!" );
    }
    this.sampleCount = aSampleCount;
    return this;
  }

  /**
   * @param aSampleCount
   * @return this builder.
   */
  public GenericConfigBuilder setSampleCount( String aSampleCount )
  {
    try
    {
      return setSampleCount( Integer.parseInt( aSampleCount ) );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid sample count!" );
    }
  }

  /**
   * @param aSampleRate
   * @return this builder.
   */
  public GenericConfigBuilder setSampleRate( int aSampleRate )
  {
    if ( aSampleRate < 1 )
    {
      throw new IllegalArgumentException( "Invalid sample rate!" );
    }
    this.sampleRate = aSampleRate;
    return this;
  }

  /**
   * @param aSampleRate
   * @return this builder.
   */
  public GenericConfigBuilder setSampleRate( String aSampleRate )
  {
    try
    {
      return setSampleRate( Integer.parseInt( aSampleRate ) );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid sample rate!" );
    }
  }

  /**
   * @param aSampleWidth
   * @return this builder.
   */
  public GenericConfigBuilder setSampleWidth( int aSampleWidth )
  {
    if ( aSampleWidth < 1 )
    {
      throw new IllegalArgumentException( "Invalid sample width!" );
    }
    this.sampleWidth = aSampleWidth;
    return this;
  }

  /**
   * @param aSampleWidth
   * @return this builder.
   */
  public GenericConfigBuilder setSampleWidth( String aSampleWidth )
  {
    try
    {
      return setSampleWidth( Integer.parseInt( aSampleWidth ) );
    }
    catch ( Exception exception )
    {
      throw new IllegalArgumentException( "Invalid sample width!" );
    }
  }
}
