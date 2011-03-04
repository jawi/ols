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
package org.sump.device.logicsniffer;


import java.util.*;

import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.sump.device.logicsniffer.profile.*;


/**
 * @author jawi
 */
public class Activator implements BundleActivator
{
  // VARIABLES

  private ServiceRegistration serviceRegistration;

  // METHODS

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    Dictionary<String, String> props = new Hashtable<String, String>();
    props.put( Constants.SERVICE_PID, DeviceProfileManager.SERVICE_PID );

    this.serviceRegistration = aContext.registerService( ManagedServiceFactory.class.getName(),
        new DeviceProfileManager(), props );

  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    if ( this.serviceRegistration != null )
    {
      this.serviceRegistration.unregister();
      this.serviceRegistration = null;
    }
  }
}
