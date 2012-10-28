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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package org.sump.device.logicsniffer.protocol;


/**
 * Provides some common used constants in the SUMP protocol.
 */
public interface SumpProtocolConstants
{
  // CONSTANTS

  /** The sample clock internally used by the OLS device, in Hertz (Hz). */
  public static final int CLOCK = 100000000; // device clock in Hz

  /** Old SLA version, v0 (0x534c4130, or 0x30414c53) - no longer supported. */
  public static final int SLA_V0 = 0x30414c53;
  /** Current SLA version, v1 (0x534c4131, or 0x31414c53) - supported. */
  public static final int SLA_V1 = 0x31414c53;

  /** reset analyzer */
  public static final int CMD_RESET = 0x00;
  /** arm trigger / run device */
  public static final int CMD_RUN = 0x01;
  /** ask for device id */
  public static final int CMD_ID = 0x02;
  /** ask for device self test. */
  public static final int CMD_SELFTEST = 0x03;
  /** ask for device meta data. */
  /** ask for device meta data. */
  public static final int CMD_METADATA = 0x04;
  /** ask the device to immediately return its RLE-encoded data. */
  public static final int CMD_RLE_FINISH_NOW = 0x05;

}
