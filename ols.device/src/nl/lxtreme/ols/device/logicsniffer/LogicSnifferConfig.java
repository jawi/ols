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
package org.sump.device.logicsniffer;


import org.sump.device.logicsniffer.profile.*;
import org.sump.device.logicsniffer.profile.DeviceProfile.*;

import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration for the generic device.
 */
@Meta.OCD( name = "Generic device" )
public interface LogicSnifferConfig
{
  // METHODS

  @Meta.AD( name = "Connection type", required = true, deflt = "SERIAL", //
  optionLabels = { "Network", "Serial port" }, optionValues = { "NETWORK", "SERIAL" } )
  DeviceInterface deviceInterface();

  @Meta.AD( name = "Remote host" )
  String remoteHost();

  @Meta.AD( name = "Remote port", min = "1", max = "65535" )
  int remotePort();

  @Meta.AD( name = "Analyzer port" )
  String localPort();

  @Meta.AD( name = "Port speed", min = "1" )
  int portSpeed();

  @Meta.AD( name = "Device profile", required = true )
  DeviceProfile deviceProfile();

  @Meta.AD( name = "Number scheme", required = true, deflt = "false", //
  optionLabels = { "Inside", "Outside" }, optionValues = { "true", "false" } )
  boolean altNumberSchemeEnabled();

  @Meta.AD( name = "Clock source", required = true, deflt = "INTERNAL", //
  optionLabels = { "Internal", "External / Falling", "External / Rising" }, optionValues = { "INTERNAL",
      "EXTERNAL_FALLING", "EXTERNAL_RISING" } )
  CaptureClockSource clockSource();

  @Meta.AD( name = "Sample rate", required = true )
  int sampleRate();

  @Meta.AD( name = "Enabled channels", required = true )
  int enabledChannels();

  @Meta.AD( name = "Recording size", required = true )
  int size();

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
