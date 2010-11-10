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
package nl.lxtreme.ols.api.devices;


import java.io.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.data.*;


/**
 * Denotes a device that can be managed by OLS.
 */
public interface Device
{
  // METHODS

  /**
   * Attaches the given (serial) port to the device object. The method will try
   * to open the port.
   * <p>
   * A return value of <code>true</code> does not guarantee that a logic
   * analyzer is actually attached to the port.
   * <p>
   * If the device is already attached to a port this port will be detached
   * automatically. It is therefore not necessary to manually call
   * <code>detach()</code> before reattaching.
   * 
   * @param aPortName
   *          the name of the port to open
   * @param aPortRate
   *          transfer rate to use (bps)
   * @return <code>true</code> when the port has been assigned successfully;
   *         <code>false</code> otherwise.
   * @throws IOException
   *           in case of I/O problems during attaching or setup of the port.
   */
  public boolean attach( final String aPortName, final int aPortRate ) throws IOException;

  /**
   * Detaches the currently attached port, if one exists. This will close the
   * serial port.
   */
  public void detach();

  /**
   * Returns this device's metadata.
   * 
   * @return the device metadata descriptor, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during reading/writing from or to the
   *           device;
   * @throws IllegalStateException
   *           in case this methods was called without being attached to a
   *           serial port.
   */
  public DeviceMetadata getMetadata() throws IOException, IllegalStateException;

  /**
   * Sends the configuration to the device, starts it, reads the captured data
   * and returns a CapturedData object containing the data read as well as
   * device configuration information.
   * 
   * @return captured data
   * @throws IOException
   *           when writing to or reading from device fails;
   * @throws InterruptedException
   *           if a read time out occurs after trigger match or stop() was
   *           called before trigger match;
   * @throws IllegalStateException
   *           in case this methods was called without being attached to a
   *           serial port.
   */
  public CapturedData run( final ProgressCallback aCallback ) throws IOException, InterruptedException,
      IllegalStateException;
}

/* EOF */
