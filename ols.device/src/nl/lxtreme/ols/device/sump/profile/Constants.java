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
package nl.lxtreme.ols.device.sump.profile;

/**
 * Commonly used constants.
 */
public interface Constants
{
  // CONSTANTS

  /** The short (single word) type of the device described in this profile */
  public static final String DEVICE_TYPE = "device.type";
  /** A longer description of the device */
  public static final String DEVICE_DESCRIPTION = "device.description";
  /** The device interface, currently SERIAL only */
  public static final String DEVICE_INTERFACE = "device.interface";
  /** The device's native clockspeed, in Hertz. */
  public static final String DEVICE_CLOCKSPEED = "device.clockspeed";
  /**
   * The clockspeed used in the divider calculation, in Hertz. Defaults to
   * 100MHz as most devices appear to use this.
   */
  public static final String DEVICE_DIVIDER_CLOCKSPEED = "device.dividerClockspeed";
  /**
   * Whether or not double-data-rate is supported by the device (also known as
   * the "demux"-mode).
   */
  public static final String DEVICE_SUPPORTS_DDR = "device.supports_ddr";
  /** Supported sample rates in Hertz, separated by comma's */
  public static final String DEVICE_SAMPLERATES = "device.samplerates";
  /** What capture clocks are supported */
  public static final String DEVICE_CAPTURECLOCK = "device.captureclock";
  /** The supported capture sizes, in bytes */
  public static final String DEVICE_CAPTURESIZES = "device.capturesizes";
  /** Whether or not the noise filter is supported */
  public static final String DEVICE_FEATURE_NOISEFILTER = "device.feature.noisefilter";
  /** Whether or not Run-Length encoding is supported */
  public static final String DEVICE_FEATURE_RLE = "device.feature.rle";
  /** Whether or not a testing mode is supported. */
  public static final String DEVICE_FEATURE_TEST_MODE = "device.feature.testmode";
  /** Whether or not the read/delay counters are combined into one value. */
  public static final String DEVICE_FEATURE_COMBINED_READDELAY_COUNT = "device.feature.combinedReadDelayCount";
  /** Whether or not triggers are supported */
  public static final String DEVICE_FEATURE_TRIGGERS = "device.feature.triggers";
  /** The number of trigger stages */
  public static final String DEVICE_TRIGGER_STAGES = "device.trigger.stages";
  /** Whether or not "complex" triggers are supported */
  public static final String DEVICE_TRIGGER_COMPLEX = "device.trigger.complex";
  /** Whether or not "HP165xx"-style triggers are supported. */
  public static final String DEVICE_TRIGGER_HP165XX = "device.trigger.hp165xx";
  /** The total number of channels usable for capturing */
  public static final String DEVICE_CHANNEL_COUNT = "device.channel.count";
  /**
   * The number of channels groups, together with the channel count determines
   * the channels per group
   */
  public static final String DEVICE_CHANNEL_GROUPS = "device.channel.groups";
  /** Whether the capture size is limited by the enabled channel groups */
  public static final String DEVICE_CAPTURESIZE_BOUND = "device.capturesize.bound";
  /** What channel numbering schemes are supported by the device. */
  public static final String DEVICE_CHANNEL_NUMBERING_SCHEMES = "device.channel.numberingschemes";
  /**
   * Is a delay after opening the port and device detection needed? (0 = no
   * delay, >0 = delay in milliseconds)
   */
  public static final String DEVICE_OPEN_PORT_DELAY = "device.open.portdelay";
  /** The receive timeout (100 = default, in milliseconds) */
  public static final String DEVICE_RECEIVE_TIMEOUT = "device.receive.timeout";
  /**
   * Which metadata keys correspond to this device profile? Value is a
   * comma-separated list of (double quoted) names.
   */
  public static final String DEVICE_METADATA_KEYS = "device.metadata.keys";
  /**
   * In which order are samples sent back from the device? If <code>true</code>
   * then last sample first, if <code>false</code> then first sample first.
   */
  public static final String DEVICE_LAST_SAMPLE_FIRST = "device.samples.lastSampleFirst";
  /**
   * In case of a serial port, does the DTR-line need to be high (= true) or low
   * (= false)?
   */
  public static final String DEVICE_OPEN_PORT_DTR = "device.open.portdtr";

}
