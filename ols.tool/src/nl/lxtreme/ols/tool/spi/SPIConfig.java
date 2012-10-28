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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.spi;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the SPI decoder tool.
 */
@Meta.OCD( name = "SPI tool configuration" )
public interface SPIConfig
{
  // METHODS

  /**
   * @return the protocol mode, never <code>null</code>.
   */
  @Meta.AD( name = "Protocol mode", description = "{listen=!io2Idx:!io2Idx:io2Idx,listen=!io3Idx:!io3Idx:io3Idx}", optionLabels = {
      "Standard", "Dual mode", "Quad mode" }, deflt = "0", required = true )
  SPIFIMode protocol();

  /**
   * Returns the index of the channel on which the /CS data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "/CS", deflt = "0", required = true )
  int csIdx();

  /**
   * Returns the index of the channel on which the SCK data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "SCK", deflt = "1", required = true )
  int sckIdx();

  /**
   * Returns the index of the channel on which the MOSI data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "MOSI (IO0)", deflt = "2", required = false )
  int mosiIdx();

  /**
   * Returns the index of the channel on which the MISO data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "MISO (IO1)", deflt = "-1", required = false )
  int misoIdx();

  /**
   * Returns the index of the channel on which the IO2 data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "IO2", deflt = "-1", required = false )
  int io2Idx();

  /**
   * Returns the index of the channel on which the IO3 data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "IO3", deflt = "-1", required = false )
  int io3Idx();

  /**
   * Returns the index of the channel on which the SDA/LineA data is present.
   * 
   * @return a SPI mode, never <code>null</code>.
   */
  @Meta.AD( name = "SPI Mode", optionLabels = { "Auto-detect", "Mode 0", "Mode 1", "Mode 2", "Mode 3" }, deflt = "2", required = true )
  SPIMode mode();

  /**
   * @return the number of bits in a SPI-word, >= 4 && <= 32.
   */
  @Meta.AD( name = "Bits", min = "4", max = "32", deflt = "8" )
  int bitCount();

  /**
   * @return
   */
  @Meta.AD( name = "Order", optionLabels = { "LSB first", "MSB first" }, deflt = "0", required = true )
  BitOrder bitOrder();

  /**
   * @return
   */
  @Meta.AD( name = "Report /CS", deflt = "true" )
  boolean reportCS();

  /**
   * @return
   */
  @Meta.AD( name = "Honour /CS", deflt = "false" )
  boolean honourCS();

}
