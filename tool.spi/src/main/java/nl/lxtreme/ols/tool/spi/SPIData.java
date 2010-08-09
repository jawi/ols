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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.spi;


/**
 * Class for SPI dataset
 * 
 * @author Frank Kunz A SPI dataset consists of a timestamp, MISO and MOSI
 *         values, or it can have an SPI event. This class is used to store the
 *         decoded SPI data in a Vector.
 */
public class SPIData
{
  // VARIABLES

  private final long time;
  private final int miso;
  private final int mosi;
  private final String event;

  // CONSTRUCTORS

  /**
   * @param tm
   * @param mo
   * @param mi
   */
  public SPIData( final long tm, final int mo, final int mi )
  {
    this.time = tm;
    this.miso = mi;
    this.mosi = mo;
    this.event = null;
  }

  /**
   * @param tm
   * @param ev
   */
  public SPIData( final long tm, final String ev )
  {
    this.time = tm;
    this.miso = 0;
    this.mosi = 0;
    this.event = new String( ev );
  }

  // METHODS

  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof SPIData ) )
    {
      return false;
    }

    final SPIData other = ( SPIData )aObject;
    if ( this.event == null )
    {
      if ( other.event != null )
      {
        return false;
      }
    }
    else if ( !this.event.equals( other.event ) )
    {
      return false;
    }

    if ( this.miso != other.miso )
    {
      return false;
    }

    if ( this.mosi != other.mosi )
    {
      return false;
    }

    if ( this.time != other.time )
    {
      return false;
    }

    return true;
  }

  /**
   * @return
   */
  public String getEvent()
  {
    return this.event;
  }

  /**
   * @return
   */
  public int getMiSoValue()
  {
    return this.miso;
  }

  /**
   * @return
   */
  public int getMoSiValue()
  {
    return this.mosi;
  }

  /**
   * @return
   */
  public long getTime()
  {
    return this.time;
  }

  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( this.event == null ) ? 0 : this.event.hashCode() );
    result = prime * result + this.miso;
    result = prime * result + this.mosi;
    result = prime * result + ( int )( this.time ^ ( this.time >>> 32 ) );
    return result;
  }

  /**
   * @return
   */
  public boolean isEvent()
  {
    return ( this.event != null );
  }
}
