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
package nl.lxtreme.ols.device.sump;


/**
 * Provides some constants for use in SUMP-compatible devices.
 */
public interface SumpConstants
{
  // CONSTANTS

  // lookups used for filters...
  String LOOKUP_DEVICE_NAME = "name";
  String LOOKUP_FPGA_VERSION = "fpgaVersion";
  String LOOKUP_ANCILLARY_VERSION = "ancillaryVersion";
  String LOOKUP_PROTOCOL_VERSION = "protocolVersion";
  String LOOKUP_PROBE_COUNT = "probeCount";
  String LOOKUP_SAMPLE_MEMORY_DEPTH = "sampleMemory";
  String LOOKUP_DYNAMIC_MEMORY_DEPTH = "dynamicMemory";
  String LOOKUP_MAX_SAMPLE_RATE = "maxSampleRate";

  // null-terminated string key values...
  int KEY_DEVICE_NAME = 0x01;
  int KEY_FPGA_VERSION = 0x02;
  int KEY_ANCILLARY_VERSION = 0x03;
  // 32-bit integer metadata key values...
  int KEY_PROBE_COUNT_LONG = 0x20;
  int KEY_SAMPLE_MEMORY_DEPTH = 0x21;
  int KEY_DYNAMIC_MEMORY_DEPTH = 0x22;
  int KEY_MAX_SAMPLE_RATE = 0x23;
  int KEY_PROTOCOL_VERSION_LONG = 0x24;
  // 8-bit integer metadata key values...
  int KEY_PROBE_COUNT_SHORT = 0x40;
  int KEY_PROTOCOL_VERSION_SHORT = 0x41;

  /** The number of trigger stages. */
  int MAX_COMPLEX_TRIGGER_STAGES = 4;

  int MIN_CHANNEL_GROUPS = 1;
  int MAX_CHANNEL_GROUPS_DDR = 2;
  int MAX_SIMPLE_TRIGGER_STAGES = 4;
  
  String TRIGGER_TYPE_BASIC = "basic";
  String TRIGGER_TYPE_ADVANCED = "advanced";

  // Configuration keys
  String KEY_CONNECTION_URI = "sump.connectionURI";
  String KEY_DIVIDER = "sump.divider";
  String KEY_ENABLED_CHANNELS = "sump.enabledChannels";
  String KEY_GROUP_COUNT = "sump.groupCount";
  String KEY_READ_COUNT = "sump.readCount";
  String KEY_DELAY_COUNT = "sump.delayCount";
  String KEY_FLAGS = "sump.flags";
  String KEY_SAMPLE_RATE = "sump.sampleRate";
  String KEY_LAST_SAMPLE_SENT_FIRST = "sump.lastSampleSentFirst";
  String KEY_READ_DELAY_COUNT_COMBINED = "sump.readDelayCountCombined";
  String KEY_TRIGGER_DEFS = "sump.triggerDefs";

}
