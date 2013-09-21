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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.device.sump.profile.provision;

import static nl.lxtreme.ols.device.sump.profile.Constants.*;

import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.device.sump.profile.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;


/**
 * Provisions all device profiles by creating or updating the managed services.
 */
public class ProfileProvisioner
{
  // CONSTANTS

  /** The PID of the ManagedServiceFactory to provision to. */
  private static final String FACTORY_PID = DeviceProfileManager.SERVICE_PID;
  /** The filter to retrieve all existing configurations. */
  private static final String FILTER = "(service.factoryPid=" + FACTORY_PID + ")";

  // VARIABLES

  // injected by Felix DM...
  private volatile BundleContext context;
  private volatile ConfigurationAdmin configAdmin;
  private volatile LogService logService;

  // METHODS

  /**
   * Called by Felix DM upon starting this component.
   * 
   * @param aComponent
   *          the component definition.
   */
  public void start( Component aComponent ) throws Exception
  {
    Map<String, Configuration> configurations = getExistingConfigurations();

    Bundle bundle = this.context.getBundle();
    Enumeration<?> entries = bundle.findEntries( "profiles", "*.cfg", false /* recurse */);
    while ( entries.hasMoreElements() )
    {
      URL entry = ( URL )entries.nextElement();

      this.logService.log( LogService.LOG_DEBUG, "Found device profile: " + entry.getFile() + "..." );

      Properties props = new Properties();
      props.load( entry.openStream() );

      String deviceType = props.getProperty( DEVICE_TYPE );

      this.logService.log( LogService.LOG_DEBUG, "Device profile: " + deviceType + " loaded..." );

      Configuration config = configurations.get( deviceType );
      if ( config == null )
      {
        config = this.configAdmin.createFactoryConfiguration( FACTORY_PID, null /* location */);

        this.logService.log( LogService.LOG_DEBUG, "Configuration for profile: " + deviceType + " created..." );
      }
      config.update( props );

      this.logService.log( LogService.LOG_INFO, "Device profile: " + deviceType + " provisioned..." );
    }
  }

  /**
   * Returns a map with all existing device-profile configurations.
   * 
   * @return a map of device-profile configurations, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems accessing the configuration storage area;
   * @throws InvalidSyntaxException
   *           in case of an invalid filter syntax.
   */
  private Map<String, Configuration> getExistingConfigurations() throws IOException, InvalidSyntaxException
  {
    Map<String, Configuration> result = new HashMap<String, Configuration>();

    Configuration[] configurations = this.configAdmin.listConfigurations( FILTER );
    if ( configurations != null )
    {
      for ( Configuration configuration : configurations )
      {
        String deviceType = ( String )configuration.getProperties().get( DEVICE_TYPE );
        result.put( deviceType, configuration );
      }
    }

    return result;
  }
}
