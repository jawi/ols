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


import nl.lxtreme.ols.tool.base.*;


/**
 * Class for SPI dataset
 * 
 * @author Frank Kunz A SPI dataset consists of a timestamp, MISO and MOSI
 *         values, or it can have an SPI event. This class is used to store the
 *         decoded SPI data in a Vector.
 */
public final class SPIData extends BaseData<SPIData>
{
  // VARIABLES

  private final int miso;
  private final int mosi;
  private final String event;

  // CONSTRUCTORS

  /**
   * @param aTime
   * @param aMoSi
   * @param aMiSo
   */
  public SPIData( final long aTime, final int aMoSi, final int aMiSo, final String aTimeDisplayValue )
  {
    super( aTime, aTimeDisplayValue );
    this.miso = aMiSo;
    this.mosi = aMoSi;
    this.event = null;
  }

  /**
   * @param aTime
   * @param aEvent
   */
  public SPIData( final long aTime, final String aEvent, final String aTimeDisplayValue )
  {
    super( aTime, aTimeDisplayValue );
    this.miso = 0;
    this.mosi = 0;
    this.event = aEvent;
  }

  // METHODS

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( !super.equals( aObject ) )
    {
      return false;
    }
    if ( !( aObject instanceof SPIData ) )
    {
      return false;
    }
    SPIData other = ( SPIData )aObject;
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
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ( ( this.event == null ) ? 0 : this.event.hashCode() );
    result = prime * result + this.miso;
    result = prime * result + this.mosi;
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
