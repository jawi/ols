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
package nl.lxtreme.ols.device.generic;


/**
 * Constants used by the demo device.
 */
public interface GenericConstants
{
  // CONSTANTS

  /**
   * The number of channels to return. The value should represent a positive,
   * non-zero, {@link Integer} value.
   */
  String KEY_CHANNEL_COUNT = "generic.channelCount";
  /**
   * The number of samples to return. The value should represent a positive,
   * non-zero, {@link Integer} value.
   */
  String KEY_SAMPLE_COUNT = "generic.sampleCount";
  /**
   * The sample rate, in Hertz (Hz). The value should represent a positive,
   * non-zero, {@link Integer} value.
   */
  String KEY_SAMPLE_RATE = "generic.sampleRate";
  /**
   * The width of one sample, in bytes (8 bits). The value should represent a
   * positive, non-zero, {@link Integer} value.
   */
  String KEY_SAMPLE_WIDTH = "generic.sampleWidth";
  /**
   * The path to the device to read the samples from. The value should represent
   * a {@link String} value to the device.
   */
  String KEY_DEVICE_PATH = "generic.devicePath";

}
