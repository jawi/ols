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
package nl.lxtreme.ols.device.api;


import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Interface for implementing device controllers. Each supported device must
 * implement at least this interface.
 */
public interface Device
{
  // METHODS

  /**
   * Starts the acquisition of data from the device and yields a promise that
   * will yield the {@link AcquisitionData}.
   * 
   * @param aConfig
   *          the device configuration to use, cannot be <code>null</code>;
   * @param aProgressListener
   *          the progress listener to use, can be <code>null</code> in case of
   *          no interest in progress updates.
   * @return a promise (future) to the acquired data, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given device config was <code>null</code> or not
   *           valid.
   * @throws IOException
   *           in case the acquisition failed, for example, due to an
   *           inaccessible device.
   */
  Future<AcquisitionData> acquireData( Map<String, ? extends Serializable> aConfig,
      AcquisitionProgressListener aProgressListener ) throws IOException;

  /**
   * Returns a descriptive name of this device controller.
   * 
   * @return name of the controller, cannot be <code>null</code>.
   */
  String getName();

  /**
   * Returns whether this device is already set up or not.
   * 
   * @return <code>true</code> if there is a "valid" setup for this device,
   *         <code>false</code> otherwise.
   */
  boolean isSetup();

  /**
   * Allows this device controller to set up the device by means of presenting
   * an UI.
   * 
   * @return the device configuration, or <code>null</code> if the user
   *         cancelled or disapproved the configuration.
   */
  Map<String, ? extends Serializable> setupDevice();
}
