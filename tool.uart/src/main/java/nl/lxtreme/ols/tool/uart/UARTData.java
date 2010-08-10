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


import nl.lxtreme.ols.tool.base.*;


/**
 * Class for UART dataset
 * 
 * @author Frank Kunz
 */
public final class UARTData extends BaseData<UARTData>
{
  // CONSTANTS

  public static final int UART_TYPE_EVENT = 0;
  public static final int UART_TYPE_RXEVENT = 1;
  public static final int UART_TYPE_TXEVENT = 2;
  public static final int UART_TYPE_RXDATA = 3;
  public static final int UART_TYPE_TXDATA = 4;

  // VARIABLES

  private final int data;
  private final int type;
  private final String event;

  // CONSTRUCTORS

  /*
   * data
   */
  public UARTData( final long aTime, final int aData, final int aType, final String aTimeDisplayValue )
  {
    super( aTime, aTimeDisplayValue );
    this.data = aData;
    this.type = aType;
    this.event = null;
  }

  /*
   * type specific event
   */
  public UARTData( final long aTime, final String aEvent, final int aType, final String aTimeDisplayValue )
  {
    super( aTime, aTimeDisplayValue );
    this.data = 0;
    this.type = aType;
    this.event = aEvent;
  }

  /*
   * generic event
   */
  public UARTData( final long aTime, final String aEvent, final String aTimeDisplayValue )
  {
    super( aTime, aTimeDisplayValue );
    this.data = 0;
    this.type = UART_TYPE_EVENT;
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
    if ( !super.equals( aObject ) || !( aObject instanceof UARTData ) )
    {
      return false;
    }

    final UARTData other = ( UARTData )aObject;
    if ( this.type != other.type )
    {
      return false;
    }

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
  public int getType()
  {
    return this.type;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + this.data;
    result = prime * result + ( ( this.event == null ) ? 0 : this.event.hashCode() );
    result = prime * result + this.type;
    return result;
  }
}
