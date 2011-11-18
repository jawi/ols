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
package org.sump.device.logicsniffer.profile;


import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.util.tracker.*;


/**
 * 
 */
public class DeviceProfileManagerTracker extends ServiceTracker
{
  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfileManagerTracker instance.
   * 
   * @param aContext
   */
  public DeviceProfileManagerTracker( final BundleContext aContext )
  {
    super( aContext, createFilter(), null /* aCustomizer */);
  }

  // METHODS

  /**
   * Creates a filter for tracking device profile managers.
   * 
   * @return a {@link Filter} instance, never <code>null</code>.
   * @throws RuntimeException
   *           in case the filter definition is invalid.
   */
  private static Filter createFilter()
  {
    try
    {
      String filter = String.format( "(&(%s=%s)(%s=%s))", Constants.SERVICE_PID, DeviceProfileManager.SERVICE_PID,
          Constants.OBJECTCLASS, ManagedServiceFactory.class.getName() );

      return FrameworkUtil.createFilter( filter );
    }
    catch ( InvalidSyntaxException exception )
    {
      throw new RuntimeException( "Invalid filter syntax?!", exception );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DeviceProfileManager getService()
  {
    return ( DeviceProfileManager )super.getService();
  }
}
