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
 * @author Frank Kunz
 *         A SPI dataset consists of a timestamp, MISO and MOSI values, or it can have
 *         an SPI event. This class is used to store the decoded SPI data in a Vector.
 */
final class SPIProtocolAnalysisDataSet
{
  // VARIABLES

  public long   time;
  public int    miso;
  public int    mosi;
  public String event;

  // CONSTRUCTORS

  public SPIProtocolAnalysisDataSet( final long tm, final int mo, final int mi )
  {
    this.time = tm;
    this.miso = mi;
    this.mosi = mo;
    this.event = null;
  }

  public SPIProtocolAnalysisDataSet( final long tm, final String ev )
  {
    this.time = tm;
    this.miso = 0;
    this.mosi = 0;
    this.event = new String( ev );
  }

  // METHODS

  public boolean isEvent()
  {
    return ( this.event != null );
  }
}
