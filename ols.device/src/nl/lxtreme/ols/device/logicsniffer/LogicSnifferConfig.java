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
package nl.lxtreme.ols.device.logicsniffer;


import nl.lxtreme.ols.device.logicsniffer.profile.DeviceProfile.*;

import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration for the generic device.
 */
@Meta.OCD( name = "Generic device" )
public interface LogicSnifferConfig
{
  // METHODS

  @Meta.AD( name = "Connection type", required = true, deflt = "SERIAL", //
  optionLabels = { "Network", "Serial port" }, optionValues = { "NETWORK", "SERIAL" }, //
  description = "{listener=remoteHost:!remoteHost,listener=remotePort:!remotePort,listener=!localPort:localPort,listener=!portSpeed:portSpeed}" )
  DeviceInterface deviceInterface();

  @Meta.AD( name = "Remote host", deflt = "localhost" )
  String remoteHost();

  @Meta.AD( name = "Remote port", min = "1", max = "65535", deflt = "5678" )
  int remotePort();

  @Meta.AD( name = "Serial port", deflt = "" )
  String localPort();

  @Meta.AD( name = "Port speed", min = "1", deflt = "115200", optionLabels = { "921600bps", "460800bps", "230400bps",
      "115200bps", "57600bps", "38400bps", "19200bps", "14400bps", "9600bps", "4800bps" }, optionValues = { "921600",
      "460800", "230400", "115200", "57600", "38400", "19200", "14400", "9600", "4800" } )
  int portSpeed();

  @Meta.AD( name = "Device profile", required = true )
  String deviceProfile();

  @Meta.AD( name = "Number scheme", required = true, deflt = "DEFAULT", //
  optionLabels = { "Default", "Inside", "Outside" }, optionValues = { "DEFAULT", "INSIDE", "OUTSIDE" } )
  String numberScheme();

  @Meta.AD( name = "Clock source", required = true, deflt = "INTERNAL", //
  optionLabels = { "Internal", "External / Falling", "External / Rising" }, optionValues = { "INTERNAL",
      "EXTERNAL_FALLING", "EXTERNAL_RISING" } )
  CaptureClockSource clockSource();

  @Meta.AD( name = "Sample rate", required = true, deflt = "1000000" )
  int sampleRate();

  @Meta.AD( name = "Enabled channels", required = true, deflt = "255" )
  int enabledChannels();

  @Meta.AD( name = "Recording size", required = true )
  int size();

  @Meta.AD( name = "Automatic (maximum)", required = true, deflt = "true" )
  boolean useMaxSize();

  @Meta.AD( name = "Test mode", required = true, deflt = "false" )
  boolean testModeEnabled();

  @Meta.AD( name = "Noise filter", required = true, deflt = "false" )
  boolean filterEnabled();

  @Meta.AD( name = "Run Length Encoding", required = true, deflt = "false" )
  boolean rleEnabled();

  @Meta.AD( name = "Enable trigger?", required = true, deflt = "false" )
  boolean triggerEnabled();

  @Meta.AD( name = "Before/After ratio", required = true, min = "0", max = "100", deflt = "50" )
  int ratio();

}
