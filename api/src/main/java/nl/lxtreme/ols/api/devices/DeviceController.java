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


/**
 * Interface for implementing device controllers. Each supported device must
 * implement at least this interface.
 */
public interface DeviceController
{
  // METHODS

  /**
   * Signals that the capture should be aborted.
   * 
   * @throws IllegalStateException
   *           in case no capture is in progress.
   */
  public void cancel() throws IllegalStateException;

  /**
   * Reads the captured device data.
   * 
   * @param aCallback
   *          the callback to use for publishing the captured data or
   *          <code>null</code> if no data available.
   * @throws IOException
   *           in case of I/O problems during the capturing of data.
   */
  public void captureData( final CaptureCallback aCallback ) throws IOException;

  /**
   * Returns the device controller identification string.
   * 
   * @return name of the controller, cannot be <code>null</code>.
   */
  public String getName();

  /**
   * Returns whether or not this device controller is in progress of a capture.
   * 
   * @return <code>true</code> if this device controller is currently capturing
   *         data (or waiting to start capturing due to a trigger),
   *         <code>false</code> otherwise.
   */
  public boolean isCapturing();

  /**
   * Returns whether this device is already set up or not.
   * 
   * @return <code>true</code> if there is a "valid" setup for this device,
   *         <code>false</code> otherwise.
   */
  public boolean isSetup();

  /**
   * Allows this device controller to set up the device by means of presenting
   * an UI.
   * 
   * @param aParent
   *          the parent window that can be used to display (modal) dialogs, can
   *          be <code>null</code>.
   * @return <code>true</code> if the setup is successfully completed (the user
   *         acknowledged the setup), <code>false</code> if the setup is aborted
   *         by the user.
   * @throws IOException
   *           in case of I/O problems.
   */
  public boolean setupCapture( final java.awt.Window aParent );
}
