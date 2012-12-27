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
package nl.lxtreme.ols.client.ui;


import nl.lxtreme.ols.client.ui.device.*;
import nl.lxtreme.ols.client.ui.device.impl.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.device.api.*;

import org.apache.felix.dm.*;
import org.osgi.service.cm.*;
import org.osgi.service.event.*;
import org.osgi.service.metatype.*;


/**
 * Provides a bundle adapter for all device-registering bundles.
 */
public class DeviceBundleAdapter extends AbstractBundleAdapter<Device>
{
  // CONSTANTS

  private static final String OLS_DEVICE_MAGIC_KEY = "OLS-Device";
  private static final String OLS_DEVICE_MAGIC_VALUE = "1.0";
  private static final String OLS_DEVICE_CLASS_KEY = "OLS-DeviceClass";

  public static final String FILTER = String.format( "(&(%s=%s)(%s=*))", OLS_DEVICE_MAGIC_KEY, OLS_DEVICE_MAGIC_VALUE,
      OLS_DEVICE_CLASS_KEY );

  // CONSTRUCTORS

  /**
   * Creates a new {@link DeviceBundleAdapter} instance.
   */
  public DeviceBundleAdapter()
  {
    super( Device.class, OLS_DEVICE_CLASS_KEY );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  protected void addServiceDependencies( final DependencyManager aManager, final Component aComponent )
  {
    aComponent.add( aManager.createServiceDependency() //
        .setService( Session.class ) //
        .setRequired( true ) ) //
        .add( aManager.createServiceDependency() //
            .setService( EventAdmin.class ) //
            .setRequired( true ) ) //
        .add( aManager.createServiceDependency() //
            .setService( MetaTypeService.class ) //
            .setRequired( true ) ) //
        .add( aManager.createServiceDependency() //
            .setService( ConfigurationAdmin.class ) //
            .setRequired( true ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Object getImplementation( final Class<Device> aType ) throws Exception
  {
    return new AcquisitionDeviceImpl( aType.newInstance() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Class<?>[] getPublishedInterfaces()
  {
    return new Class<?>[] { AcquisitionDevice.class, ManagedService.class };
  }
}
