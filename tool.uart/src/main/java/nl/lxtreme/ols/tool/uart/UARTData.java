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
package nl.lxtreme.ols.tool.uart;


/**
 * Class for UART dataset
 * 
 * @author Frank Kunz
 */
public final class UARTData implements Comparable<UARTData>
{
  // CONSTANTS

  public static final int UART_TYPE_EVENT = 0;
  public static final int UART_TYPE_RXEVENT = 1;
  public static final int UART_TYPE_TXEVENT = 2;
  public static final int UART_TYPE_RXDATA = 3;
  public static final int UART_TYPE_TXDATA = 4;

  // VARIABLES

  private final long time;
  private final int data;
  private final int type;
  private final String event;

  // CONSTRUCTORS

  /*
   * data
   */
  public UARTData( final long time, final int data, final int type )
  {
    this.time = time;
    this.data = data;
    this.type = type;
    this.event = null;
  }

  /*
   * generic event
   */
  public UARTData( final long time, final String ev )
  {
    this.time = time;
    this.data = 0;
    this.type = UART_TYPE_EVENT;
    this.event = new String( ev );
  }

  /*
   * type specific event
   */
  public UARTData( final long time, final String ev, final int type )
  {
    this.time = time;
    this.data = 0;
    this.type = type;
    this.event = new String( ev );
  }

  // METHODS

  /*
   * for result sort algo (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( final UARTData cmp )
  {
    return ( int )( this.time - cmp.time );
  }

  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof UARTData ) )
    {
      return false;
    }

    final UARTData other = ( UARTData )aObject;
    if ( this.data != other.data )
    {
      return false;
    }

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

    if ( this.type != other.type )
    {
      return false;
    }

    return true;
  }

  /**
   * @return
   */
  public int getData()
  {
    return this.data;
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
  public long getTime()
  {
    return this.time;
  }

  /**
   * @return
   */
  public int getType()
  {
    return this.type;
  }

  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + this.data;
    result = prime * result + ( ( this.event == null ) ? 0 : this.event.hashCode() );
    result = prime * result + ( int )( this.time ^ ( this.time >>> 32 ) );
    result = prime * result + this.type;
    return result;
  }
}
