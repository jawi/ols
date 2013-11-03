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

  /** demultiplex */
  int FLAG_DEMUX = 0x00000001;
  /** noise filter */
  int FLAG_FILTER = 0x00000002;
  /** channel group 1 enabled? */
  int FLAG_GROUP1_DISABLED = 0x00000004;
  /** channel group 2 enabled? */
  int FLAG_GROUP2_DISABLED = 0x00000008;
  /** channel group 3 enabled? */
  int FLAG_GROUP3_DISABLED = 0x00000010;
  /** channel group 4 enabled? */
  int FLAG_GROUP4_DISABLED = 0x00000020;
  /** external trigger? */
  int FLAG_EXTERNAL = 0x00000040;
  /** inverted */
  int FLAG_INVERTED = 0x00000080;
  /** run length encoding */
  int FLAG_RLE = 0x00000100;
  int FLAG_RLE_MODE_0 = 0x00000000;
  int FLAG_RLE_MODE_1 = 0x00004000;
  int FLAG_RLE_MODE_2 = 0x00008000;
  int FLAG_RLE_MODE_3 = 0x0000C000;
  /** Number Scheme */
  int FLAG_NUMBER_SCHEME = 0x00000200;
  /** Testing mode (external, bits 16:32) */
  int FLAG_EXTERNAL_TEST_MODE = 0x00000400;
  /** Testing mode (internal) */
  int FLAG_INTERNAL_TEST_MODE = 0x00000800;

  // Configuration keys
  String KEY_CHANNEL_COUNT = "sump.channelCount";
  String KEY_CONNECTION_URI = "sump.connectionURI";
  String KEY_DIVIDER = "sump.divider";
  String KEY_ENABLED_CHANNELS = "sump.enabledChannels";
  String KEY_GROUP_COUNT = "sump.groupCount";
  String KEY_READ_COUNT = "sump.readCount";
  String KEY_DELAY_COUNT = "sump.delayCount";
  String KEY_FLAGS = "sump.flags";
  String KEY_SAMPLE_RATE = "sump.sampleRate";
  String KEY_SAMPLES_READ_BACKWARD = "sump.sampleReadBackward";
  String KEY_READ_DELAY_COUNT_COMBINED = "sump.readDelayCountCombined";
  String KEY_TRIGGER_ENABLED = "sump.triggerEnabled";
  String KEY_TRIGGER_STAGES = "sump.triggerStages";
  /** "complex" or "hp165xx" */
  String KEY_TRIGGER_TYPE = "sump.triggerType";
  String KEY_TRIGGER_CONFIG = "sump.triggerConfig";
  String KEY_TRIGGER_MASK = "sump.triggerMask";
  String KEY_TRIGGER_VALUE = "sump.triggerValue";

}
