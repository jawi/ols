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

import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;


/**
 * Interface for implementing device controllers. Each supported device must
 * implement at least this interface.
 */
public interface Device
{
  // METHODS

  /**
   * Acquires the data from the device using the given context.
   * 
   * @param aConfiguration
   *          the (acquisition) configuration of the device, cannot be
   *          <code>null</code>;
   * @param aProgressListener
   *          the callback to report the progress of the acquisition to, cannot
   *          be <code>null</code>.
   * @return the acquired data, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the acquisition.
   */
  AcquisitionData acquireData( Configuration aConfiguration, DeviceProgressListener aProgressListener )
      throws IOException, InterruptedException;

  /**
   * Cancels an ongoing acquisition, allowing this device to perform cleanups of
   * resources and so on.
   * 
   * @throws IllegalStateException
   *           in case no acquisition is currently going on.
   */
  void cancelAcquisition() throws IllegalStateException;

  /**
   * Returns a descriptive name of this device controller.
   * 
   * @return name of the controller, cannot be <code>null</code>.
   */
  public String getName();
}
