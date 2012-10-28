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
package nl.lxtreme.ols.tool.uart.impl;


import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.Parity;
import nl.lxtreme.ols.tool.uart.AsyncSerialDataDecoder.StopBits;
import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the UART decoder tool.
 */
@Meta.OCD( name = "UART tool configuration" )
public interface UARTConfig
{
  // METHODS

  /**
   * Returns the index of the channel on which the RxD data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "RxD", required = false, deflt = "-1" )
  int rxdIdx();

  /**
   * Returns the index of the channel on which the TxD data is present.
   * 
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "TxD", required = false, deflt = "-1" )
  int txdIdx();

  /**
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "CTS", required = false, deflt = "-1" )
  int ctsIdx();

  /**
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "RTS", required = false, deflt = "-1" )
  int rtsIdx();

  /**
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "DTR", required = false, deflt = "-1" )
  int dtrIdx();

  /**
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "DSR", required = false, deflt = "-1" )
  int dsrIdx();

  /**
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "DCD", required = false, deflt = "-1" )
  int dcdIdx();

  /**
   * @return a channel index, >= 0.
   */
  @Meta.AD( name = "RI", required = false, deflt = "-1" )
  int riIdx();

  /**
   * @return
   */
  @Meta.AD( name = "Auto-detect baudrate", description = "{listen=!baudrate}", deflt = "false" )
  boolean autodetectBaudrate();

  /**
   * Returns the presumed baudrate of the UART data.
   * 
   * @return a baudrate, > 0.
   */
  @Meta.AD( name = "Baudrate", description = "{editable=true}", optionValues = { "150", "300", "600", "1200", "2400",
      "4800", "9600", "14400", "19200", "28800", "38400", "57600", "76800", "115200", "230400", "460800", "921600" }, deflt = "6" )
  int baudrate();

  /**
   * @return the parity of the data, never <code>null</code>.
   */
  @Meta.AD( name = "Parity", optionLabels = { "None", "Odd", "Even" }, deflt = "0" )
  Parity parity();

  /**
   * @return the bit count, > 0.
   */
  @Meta.AD( name = "Bits", optionValues = { "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16" }, deflt = "3" )
  int bitCount();

  /**
   * @return the number of stop bits, never <code>null</code>.
   */
  @Meta.AD( name = "Stop bits", optionLabels = { "1", "1.5", "2" }, deflt = "0" )
  StopBits stopBits();

  /**
   * @return <code>true</code> if the signal levels are inverted,
   *         <code>false</code> if not.
   */
  @Meta.AD( name = "Inverted", deflt = "false" )
  boolean inverted();

  /**
   * @return <code>true</code> if the most-significant bit comes first,
   *         <code>false</code> if the least-significant bit comes first.
   */
  @Meta.AD( name = "Inverse bit-order", deflt = "false" )
  boolean msbFirst();
}
