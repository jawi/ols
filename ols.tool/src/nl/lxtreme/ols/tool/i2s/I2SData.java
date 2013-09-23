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
package nl.lxtreme.ols.tool.i2s;


import nl.lxtreme.ols.api.data.*;


/**
 * Class for I2C dataset
 * 
 * @author Frank Kunz An I2C dataset consists of a timestamp, a value, or it can
 *         have an I2C event. This class is used to store the decoded I2C data
 *         in a Vector.
 */
public final class I2SData extends BaseData<I2SData>
{
  // VARIABLES

  private final Channel channel;
  private final long value;

  // CONSTRUCTORS

  /**
   * @param aTime
   * @param aEventName
   */
  public I2SData( final int aIdx, final int aChannelIdx, final int aSampleIdx, final Channel aChannel,
      final String aEventName )
  {
    super( aIdx, aChannelIdx, aSampleIdx, aEventName );
    this.channel = aChannel;
    this.value = 0;
  }

  /**
   * @param aTime
   * @param aValue
   */
  public I2SData( final int aIdx, final int aChannelIdx, final int aStartSampleIdx, final int aEndSampleIdx,
      final Channel aChannel, final long aValue )
  {
    super( aIdx, aChannelIdx, aStartSampleIdx, aEndSampleIdx );
    this.channel = aChannel;
    this.value = aValue;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( Object obj )
  {
    if ( this == obj )
    {
      return true;
    }
    if ( !super.equals( obj ) || getClass() != obj.getClass() )
    {
      return false;
    }

    I2SData other = ( I2SData )obj;
    if ( this.channel != other.channel )
    {
      return false;
    }
    if ( this.value != other.value )
    {
      return false;
    }
    return true;
  }

  /**
   * Returns the current value of channel.
   * 
   * @return the channel
   */
  public Channel getChannel()
  {
    return this.channel;
  }

  /**
   * Returns the value.
   * 
   * @return the value, never <code>null</code>.
   */
  public long getValue()
  {
    return this.value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ( ( this.channel == null ) ? 0 : this.channel.hashCode() );
    result = prime * result + ( int )( this.value ^ ( this.value >>> 32 ) );
    return result;
  }
}
