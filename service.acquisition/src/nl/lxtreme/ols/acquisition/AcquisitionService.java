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
package nl.lxtreme.ols.acquisition;


import java.io.*;
import java.util.*;


/**
 * Denotes a service for acquiring data from a device.
 */
public interface AcquisitionService
{
  // METHODS

  /**
   * Acquires data from the given device using the last known configuration.
   * 
   * @param aDeviceName
   *          the name of the device from which data should be acquired, cannot
   *          be <code>null</code>;
   * @throws IOException
   *           in case of I/O problems during the acquisition of data;
   * @throws IllegalArgumentException
   *           in case the given device was <code>null</code> or did not resolve
   *           to an existing device.
   * @throws IllegalStateException
   *           in case no configuration was known for the given device.
   */
  void acquireData( String aDeviceName ) throws IOException;

  /**
   * Acquires data from the given device using the given configuration.
   * 
   * @param aConfig
   *          the device configuration to use, cannot be <code>null</code>;
   * @param aDeviceName
   *          the name of the device from which data should be acquired, cannot
   *          be <code>null</code>;
   * @throws IOException
   *           in case of I/O problems during the acquisition of data;
   * @throws IllegalArgumentException
   *           in case the given configuration or device was <code>null</code>
   *           or did not resolve to an exisiting device.
   */
  void acquireData( Map<String, ? extends Serializable> aConfig, String aDeviceName ) throws IOException;

  /**
   * Configures the device with the given name.
   * 
   * @param aParent
   *          the parent window to use, can be <code>null</code>;
   * @param aDeviceName
   *          the name of the device from which data should be acquired, cannot
   *          be <code>null</code>;
   * @return the device configuration, or <code>null</code> if the device is not
   *         set up (for example, due to the user cancelling the dialog).
   * @throws IllegalArgumentException
   *           in case the given configuration or device was <code>null</code>
   *           or did not resolve to an exisiting device.
   */
  Map<String, ? extends Serializable> configureDevice( java.awt.Window aParent, String aDeviceName );

  /**
   * Signals that the current acquisition should be cancelled.
   * 
   * @param aDeviceName
   *          the name of the device to cancel the acquisition for, cannot be
   *          <code>null</code>;
   * @throws IllegalArgumentException
   *           in case the given device was <code>null</code> or did not resolve
   *           to an existing device.
   * @throws IOException
   *           in case of I/O problems during the acquisition of data;
   * @throws IllegalStateException
   *           in case no acquisition is in progress.
   */
  void cancelAcquisition( String aDeviceName ) throws IOException, IllegalStateException;

  /**
   * Returns whether or not this device controller is acquiring data for a given
   * device.
   * 
   * @param aDeviceName
   *          the name of the device to test whether it is acquiring data,
   *          cannot be <code>null</code>;
   * @return <code>true</code> if this device controller is currently acquiring
   *         data (or waiting to start capturing due to a trigger),
   *         <code>false</code> otherwise.
   */
  boolean isAcquiring( String aDeviceName );

  /**
   * Returns whether or not the given device is set up.
   * 
   * @param aDeviceName
   *          the name of the device to test whether it is set up, cannot be
   *          <code>null</code>;
   * @return <code>true</code> if this device properly set up,
   *         <code>false</code> otherwise.
   */
  boolean isDeviceSetup( String aDeviceName );
}
