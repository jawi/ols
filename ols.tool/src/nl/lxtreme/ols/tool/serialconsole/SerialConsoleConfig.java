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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.serialconsole;


import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the serial console.
 */
@Meta.OCD( name = "Serial console configuration" )
public interface SerialConsoleConfig
{
  // METHODS

  @Meta.AD( name = "Port name", required = true )
  String port();

  @Meta.AD( name = "Port speed", deflt = "8", required = true, optionLabels = { "921600bps", "460800bps", "230400bps",
      "115200bps", "57600bps", "38400bps", "19200bps", "14400bps", "9600bps", "4800bps", "2400bps" }, optionValues = {
      "921600", "460800", "230400", "115200", "57600", "38400", "19200", "14400", "9600", "4800", "2400" } )
  int baudrate();

  @Meta.AD( name = "Data bits", deflt = "3", required = true, optionLabels = { "5 bit", "6 bit", "7 bit", "8 bit",
      "9 bit" }, optionValues = { "5", "6", "7", "8", "9" } )
  int dataBits();

  @Meta.AD( name = "Parity", deflt = "0", required = true, optionLabels = { "None", "Odd", "Even" }, optionValues = {
      "none", "odd", "even" } )
  String parity();

  @Meta.AD( name = "Stop bits", deflt = "0", required = true, optionLabels = { "1 bit", "1.5 bit", "2 bits" }, optionValues = {
      "1", "1.5", "2" } )
  String stopBits();

  @Meta.AD( name = "Flow control", deflt = "0", required = true, optionLabels = { "Off", "XON/XOFF (software)",
      "RTS/CTS (hardware)" }, optionValues = { "off", "xon_xoff", "rts_cts" } )
  String flowControl();
}
