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
package nl.lxtreme.ols.acquisition.service;


import java.io.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;


/**
 * Denotes a service for acquiring data from a device.
 */
public interface DataAcquisitionService
{
  // CONSTANTS

  /** The topic prefix */
  String TOPIC_PREFIX = DataAcquisitionService.class.getPackage().getName().replaceAll( "\\.", "/" );

  /** The topic used to report progress of the acquisition data. */
  String TOPIC_ACQUISITION_PROGRESS = TOPIC_PREFIX + "/progress";

  String KEY_PROGRESS = "progress";

  /** The topic used to report the status of the acquisition. */
  String TOPIC_ACQUISITION_STATUS = TOPIC_PREFIX + "/status";

  /** All topics. */
  String TOPIC_ANY = TOPIC_PREFIX + "/*";

  /**
   * Provides information about the status of the acquisition. The value is of
   * type String and equals to one of the <tt>STATUS_*</tt> values defined in
   * this same interface.
   */
  String KEY_STATUS = "status";
  /**
   * Provides information about failures during an acquisition. The value is of
   * type {@link Exception}.
   */
  String KEY_EXCEPTION = "exception";
  /**
   * Provides information about the device which is acquiring. The value is of
   * type String and equals to the name of the device.
   */
  String KEY_DEVICE = "device";
  /**
   * Provides information about when the acquisition was started. The value is
   * of type Long and denotes the time in milliseconds (Epoch time).
   */
  String KEY_START_TIME = "startTime";

  String STATUS_STARTED = "started";
  String STATUS_FAILED = "failed";
  String STATUS_CANCELLED = "aborted";
  String STATUS_SUCCESS = "success";

  // METHODS

  /**
   * Acquires data from the given device.
   * 
   * @param aDevice
   *          the device from which data should be acquired, cannot be
   *          <code>null</code>;
   * @throws IOException
   *           in case of I/O problems during the acquisition of data;
   * @throws IllegalArgumentException
   *           in case the given device was <code>null</code>.
   */
  Future<AcquisitionData> acquireData( Device aDevice ) throws IOException;
}
