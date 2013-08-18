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
package nl.lxtreme.ols.api.devices;


/**
 * Provides some (optional) metadata of a device, as might be used by devices
 * made by Dangerous Prototypes.
 * <p>
 * This interface provides direct access to the individual metadata values. In
 * addition, it can be iterated to access all "known" metadata values.
 * </p>
 */
public interface DeviceMetadata extends Iterable<Object>
{
  // CONSTANTS

  // null-terminated string key values...
  public static final int KEY_DEVICE_NAME = 0x01;
  public static final int KEY_FPGA_VERSION = 0x02;
  public static final int KEY_ANCILLARY_VERSION = 0x03;
  // 32-bit integer metadata key values...
  public static final int KEY_PROBE_COUNT_LONG = 0x20;
  public static final int KEY_SAMPLE_MEMORY_DEPTH = 0x21;
  public static final int KEY_DYNAMIC_MEMORY_DEPTH = 0x22;
  public static final int KEY_MAX_SAMPLE_RATE = 0x23;
  public static final int KEY_PROTOCOL_VERSION_LONG = 0x24;
  // 8-bit integer metadata key values...
  public static final int KEY_PROBE_COUNT_SHORT = 0x40;
  public static final int KEY_PROTOCOL_VERSION_SHORT = 0x41;

  // METHODS

  /**
   * Returns the ancillary (e.g. PIC controller) version.
   * 
   * @return a version string, like "1.23", can be <code>null</code> in case no
   *         such version was available/known.
   */
  String getAncillaryVersion();

  /**
   * Returns the maximum depth of the dynamic memory part.
   * 
   * @return a memory depth, in bytes, can be <code>null</code> in case this
   *         depth is unknown.
   */
  Integer getDynamicMemoryDepth();

  /**
   * Returns the FPGA version.
   * 
   * @return a version string, like "1.12", can be <code>null</code> in case no
   *         such version was available/known.
   */
  String getFpgaVersion();

  /**
   * Returns the maximum sample rate.
   * 
   * @return a sample rate, in Hz, can be <code>null</code> in case the maximum
   *         sample rate is unknown.
   */
  Integer getMaxSampleRate();

  /**
   * Returns the device name.
   * 
   * @return a device name, can be <code>null</code> in case no device name is
   *         available/known.
   */
  String getName();

  /**
   * Returns the maximum number of probes supported by the device.
   * 
   * @return a probe count, >= 0 && < 256, can be <code>null</code> in case no
   *         probe count is available/known.
   */
  Integer getProbeCount();

  /**
   * Returns the device protocol version.
   * 
   * @return a protocol version, or <code>null</code> in case no protocol
   *         version is available/known.
   */
  Integer getProtocolVersion();

  /**
   * Returns the maximum sample memory.
   * 
   * @return the sample memory depth, in bytes, can be <code>null</code> in case
   *         no sample memory depth is available/known.
   */
  Integer getSampleMemoryDepth();
}
