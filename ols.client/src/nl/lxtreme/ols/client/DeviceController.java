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
package nl.lxtreme.ols.client;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.device.api.*;

import org.osgi.service.log.*;


/**
 * Provides a front-end controller for accessing device(-related) information.
 */
public final class DeviceController
{
  // VARIABLES

  private final ConcurrentMap<String, Device> devices;

  // Injected by Felix DM...
  private volatile LogService logService;

  // Holds the current selected device...
  private volatile String currentDevice;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DeviceController} instance.
   */
  public DeviceController()
  {
    this.devices = new ConcurrentHashMap<String, Device>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  public Device getDevice( final String aName )
  {
    if ( aName == null )
    {
      throw new IllegalArgumentException( "Name cannot be null!" );
    }
    return this.devices.get( aName );
  }

  /**
   * {@inheritDoc}
   */
  public String[] getDeviceNames()
  {
    List<String> result = new ArrayList<String>( this.devices.keySet() );
    // Make sure we've got a predictable order of names...
    Collections.sort( result );

    return result.toArray( new String[result.size()] );
  }

  /**
   * Returns the current selected device.
   * 
   * @return the selected device, can be <code>null</code> if no device is
   *         selected.
   */
  public Device getSelectedDevice()
  {
    return ( this.currentDevice == null ) ? null : getDevice( this.currentDevice );
  }

  /**
   * Returns the name of the current selected device.
   * 
   * @return the current device name, can be <code>null</code> if no device is
   *         selected.
   */
  public String getSelectedDeviceName()
  {
    return this.currentDevice;
  }

  /**
   * Sets the name of the current selected device.
   * 
   * @param aDeviceName
   *          the name of device to select, can be <code>null</code> if no
   *          device is to be selected.
   */
  public void setSelectedDeviceName( final String aDeviceName )
  {
    this.currentDevice = aDeviceName;

    getActionManager().updateActionStates();
  }

  /**
   * Adds a given device to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to add, cannot be <code>null</code>.
   */
  final void addDevice( final Device aDevice )
  {
    ActionManager actionManager = getActionManager();

    String deviceName = aDevice.getName();
    if ( this.devices.putIfAbsent( deviceName, aDevice ) == null )
    {
      actionManager.add( new SelectDeviceAction( this, deviceName ) );

      this.logService.log( LogService.LOG_INFO, "Added device '" + deviceName + "' ..." );
    }
    else
    {
      this.logService.log( LogService.LOG_INFO, "Add of device '" + deviceName + "' failed!" );
    }

    actionManager.updateActionStates();
  }

  /**
   * Removes a given device from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to remove, cannot be <code>null</code>.
   */
  final void removeDevice( final Device aDevice )
  {
    ActionManager actionManager = getActionManager();

    String deviceName = aDevice.getName();
    if ( this.devices.remove( deviceName, aDevice ) )
    {
      this.logService.log( LogService.LOG_INFO, "Removed device '" + deviceName + "' ..." );

      actionManager.remove( SelectDeviceAction.getID( deviceName ) );
    }
    else
    {
      this.logService.log( LogService.LOG_INFO, "Removing device '" + deviceName + "' failed!" );
    }

    actionManager.updateActionStates();
  }

  /**
   * @return the current action manager, never <code>null</code>.
   */
  private ActionManager getActionManager()
  {
    return Client.getInstance().getActionManager();
  }
}
