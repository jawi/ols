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


import java.util.*;

import org.osgi.service.cm.*;


/**
 * @author jawi
 */
public class DeviceProfileFactory implements ManagedServiceFactory
{
  // CONSTANTS

  public static final String SERVICE_PID = "ols.profile";

  // VARIABLES

  private final Map<String, DeviceProfile> profiles;

  // CONSTRUCTORS

  /**
   * Creates a new DeviceProfileFactory instance.
   */
  public DeviceProfileFactory()
  {
    this.profiles = new HashMap<String, DeviceProfile>();
  }

  // METHODS

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#deleted(java.lang.String)
   */
  @Override
  public void deleted( final String aPid )
  {
    synchronized ( this.profiles )
    {
      this.profiles.remove( aPid );
    }
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#getName()
   */
  @Override
  public String getName()
  {
    return "LogicSniffer device profile factory";
  }

  /**
   * @see org.osgi.service.cm.ManagedServiceFactory#updated(java.lang.String,
   *      java.util.Dictionary)
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final String aPid, final Dictionary aProperties ) throws ConfigurationException
  {
    synchronized ( this.profiles )
    {
      if ( this.profiles.containsKey( aPid ) )
      {
        DeviceProfile profile = this.profiles.get( aProperties );
        profile.setProperties( aProperties );
      }
      else
      {
        DeviceProfile profile = new DeviceProfile();
        this.profiles.put( aPid, profile );
        profile.setProperties( aProperties );
      }
    }
  }
}
