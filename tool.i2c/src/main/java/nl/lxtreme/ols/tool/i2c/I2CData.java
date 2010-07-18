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
package nl.lxtreme.ols.tool.i2c;


/**
 * Class for I2C dataset
 * 
 * @author Frank Kunz
 *         An I2C dataset consists of a timestamp, a value, or it can have
 *         an I2C event. This class is used to store the decoded I2C data in a Vector.
 */
final class I2CData
{
  // VARIABLES

  private final long   time;
  private final int    value;
  private final String event;

  // CONSTRUCTORS

  public I2CData( final long tm, final int val )
  {
    this.time = tm;
    this.value = val;
    this.event = null;
  }

  public I2CData( final long tm, final String ev )
  {
    this.time = tm;
    this.value = 0;
    this.event = new String( ev );
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
    if ( ( aObject == null ) || ( getClass() != aObject.getClass() ) )
    {
      return false;
    }

    final I2CData other = ( I2CData )aObject;
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

    if ( this.time != other.time )
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
   * Returns the event.
   * 
   * @return the event, never <code>null</code>.
   */
  public String getEvent()
  {
    return this.event;
  }

  /**
   * Returns the time.
   * 
   * @return the time, never <code>null</code>.
   */
  public long getTime()
  {
    return this.time;
  }

  /**
   * Returns the value.
   * 
   * @return the value, never <code>null</code>.
   */
  public int getValue()
  {
    return this.value;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( this.event == null ) ? 0 : this.event.hashCode() );
    result = prime * result + ( int )( this.time ^ ( this.time >>> 32 ) );
    result = prime * result + this.value;
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
