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


import java.io.*;

import nl.lxtreme.ols.api.acquisition.*;


/**
 * Interface for implementing device controllers. Each supported device must
 * implement at least this interface.
 */
public interface Device extends Closeable
{
  // METHODS

  /**
   * Creates a new {@link AcquisitionTask} for acquiring data from the device.
   * 
   * @param aProgressListener
   *          the acquisition progress listener the acquisition task can use to
   *          report its progress, cannot be <code>null</code>.
   * @return a new acquisition task, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the creation of the acquisition
   *           task.
   */
  public AcquisitionTask createAcquisitionTask( AcquisitionProgressListener aProgressListener ) throws IOException;

  /**
   * Creates a new {@link CancelTask} for canceling the current acquisition from
   * the device, if the device needs something special to do this.
   * 
   * @return a new cancel task, if <code>null</code> the running acquisition is
   *         simply cancelled.
   * @throws IOException
   *           in case of I/O problems during the creating of the cancellation
   *           task.
   */
  public CancelTask createCancelTask() throws IOException;

  /**
   * Returns a descriptive name of this device controller.
   * 
   * @return name of the controller, cannot be <code>null</code>.
   */
  public String getName();

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
   */
  public boolean setupCapture( final java.awt.Window aParent );
}
