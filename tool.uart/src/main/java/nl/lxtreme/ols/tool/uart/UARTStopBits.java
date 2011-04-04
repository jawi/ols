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
package nl.lxtreme.ols.tool.uart;


/**
 *
 */
public enum UARTStopBits
{
  STOP_1, STOP_15, STOP_2;

  // METHODS

  /**
   * @param aValue
   * @return
   */
  public static UARTStopBits parse( final Object aValue )
  {
    if ( "1".equals( aValue ) )
    {
      return STOP_1;
    }
    else if ( "1.5".equals( aValue ) )
    {
      return STOP_15;
    }
    else if ( "2".equals( aValue ) )
    {
      return STOP_2;
    }
    throw new IllegalArgumentException( "Unknown stop value: " + aValue );
  }

  /**
   * @return
   */
  public double getValue()
  {
    if ( this == STOP_15 )
    {
      return 1.25;
    }
    else if ( this == STOP_2 )
    {
      return 2.0;
    }
    return 1.0;
  }
}
