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
package nl.lxtreme.ols.client.osgi;


import java.util.logging.*;

import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * Provides an OSGi service tracker for device controllers.
 */
public class DeviceControllerTracker extends ServiceTracker
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( DeviceControllerTracker.class.getName() );

  // VARIABLES

  private final ClientController controller;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceControllerTracker instance.
   * 
   * @param aContext
   *          the bundle context to use;
   * @param aController
   *          the client controller to use.
   */
  public DeviceControllerTracker( final BundleContext aContext, final ClientController aController )
  {
    super( aContext, DeviceController.class.getName(), null );

    this.controller = aController;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    DeviceController devCtrl = null;

    try
    {
      devCtrl = ( DeviceController )this.context.getService( aReference );

      this.controller.addDevice( devCtrl );
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Device controller service not added! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }

    return devCtrl;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    try
    {
      final DeviceController devCtrl = ( DeviceController )aService;

      super.removedService( aReference, devCtrl );

      this.controller.removeDevice( devCtrl );
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Device controller service not removed! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }
  }
}
