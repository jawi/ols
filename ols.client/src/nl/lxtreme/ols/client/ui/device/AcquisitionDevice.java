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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui.device;


import java.awt.*;
import java.io.*;

import org.osgi.service.cm.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.device.api.*;


/**
 * Provides an adapter for {@link Device}s to allow them to be configured from
 * the UI.
 */
public interface AcquisitionDevice extends ManagedService
{
  // METHODS

  /**
   * Starts an acquisition session for this device.
   * 
   * @param aProgressListener
   *          the callback to report the progress to, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of errors during the data acquisition.
   */
  AcquisitionData acquireData( DeviceProgressListener aProgressListener ) throws IOException, InterruptedException;

  /**
   * Cancels an ongoing acquisition, allowing this device to perform cleanups of
   * resources and so on.
   * 
   * @throws IllegalStateException
   *           in case no acquisition is currently going on.
   */
  void cancelAcquisition() throws IllegalStateException;

  /**
   * Configures this device by showing a configuration dialog on screen.
   * 
   * @param aParent
   *          the parent window to use for the dialog to show.
   * @return <code>true</code> if the tool was correctly configured,
   *         <code>false</code> otherwise (user pressed cancel).
   */
  boolean configure( Window aParent );

  /**
   * Returns whether this device is already set up or not.
   * 
   * @return <code>true</code> if there is a "valid" setup for this device,
   *         <code>false</code> otherwise.
   */
  public boolean isSetup();

  /**
   * Is called to get the name for the menu entry.
   * <p>
   * The name must be unique among all tools. Should end in "..." if it opens a
   * dialog window.
   * </p>
   * 
   * @return name for this tool
   */
  String getName();

}
