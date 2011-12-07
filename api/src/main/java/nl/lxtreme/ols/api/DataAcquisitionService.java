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
package nl.lxtreme.ols.api;


import java.io.*;

import nl.lxtreme.ols.api.devices.*;


/**
 * Denotes a service for acquiring data from a device.
 */
public interface DataAcquisitionService
{
  // METHODS

  /**
   * Signals that the current acquisition should be cancelled.
   * 
   * @param aDevice
   *          the device from which data should be acquired, cannot be
   *          <code>null</code>;
   * @throws IOException
   *           in case of I/O problems during the acquisition of data;
   * @throws IllegalStateException
   *           in case no acquisition is in progress.
   */
  public void cancelAcquisition( Device aDevice ) throws IOException, IllegalStateException;

  /**
   * Returns whether or not this device controller is acquiring data.
   * 
   * @return <code>true</code> if this device controller is currently acquiring
   *         data (or waiting to start capturing due to a trigger),
   *         <code>false</code> otherwise.
   */
  public boolean isAcquiring();

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
  void acquireData( Device aDevice ) throws IOException;
}
