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
package nl.lxtreme.ols.client2.config;


import java.io.*;
import java.net.*;
import java.util.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;


/**
 * Provides a configuration provisioning service that bootstraps the OLS client
 * with the initial set of configurations.
 */
public class ConfigProvisioner
{
  // CONSTANTS

  /** The PID of the ManagedServiceFactory to provision to. */
  private static final String COLOR_SCHEME_FACTORY_PID = "ols.ui.colorscheme";
  /** The filter to retrieve all existing configurations. */
  private static final String COLOR_SCHEME_FILTER = "(service.factoryPid=" + COLOR_SCHEME_FACTORY_PID + ")";
  /** The PID of the ManagedServiceFactory to provision to. */
  private static final String UI_DEFAULTS_PID = "ols.ui.defaults";
  /** Name of the color scheme (mandatory key). */
  private static final String COLOR_SCHEME_NAME = "ols.color.scheme.name";

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
    Enumeration<?> entries = bundle.findEntries( "resources", "*.cfg", false /* recurse */);
    while ( entries.hasMoreElements() )
    {
      URL entry = ( URL )entries.nextElement();

      String name = entry.getFile();
      name = name.substring( 11 ); // strip off the '/resources/'

      this.logService.log( LogService.LOG_DEBUG, "Found: " + name + "..." );

      Properties props = new Properties();
      props.load( entry.openStream() );

      if ( name.startsWith( COLOR_SCHEME_FACTORY_PID ) )
      {
        // Color profile...
        String schemeName = props.getProperty( COLOR_SCHEME_NAME );

        Configuration config = configurations.get( schemeName );
        if ( config == null )
        {
          config = this.configAdmin.createFactoryConfiguration( COLOR_SCHEME_FACTORY_PID, null /* location */);

          this.logService.log( LogService.LOG_DEBUG, "Color scheme '" + schemeName + "' created..." );
        }
        config.update( props );

        this.logService.log( LogService.LOG_INFO, "Color scheme '" + schemeName + "' provisioned..." );
      }
      else if ( name.startsWith( UI_DEFAULTS_PID ) )
      {
        // Default UI settings...
        Configuration config = this.configAdmin.getConfiguration( UI_DEFAULTS_PID, null /* location */);
        config.update( props );

        this.logService.log( LogService.LOG_INFO, "UI defaults provisioned..." );
      }
      else
      {
        this.logService.log( LogService.LOG_WARNING, "Unknown configuration resource: " + entry.getFile() );
      }
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

    Configuration[] configurations = this.configAdmin.listConfigurations( COLOR_SCHEME_FILTER );
    if ( configurations != null )
    {
      for ( Configuration configuration : configurations )
      {
        String name = ( String )configuration.getProperties().get( COLOR_SCHEME_NAME );
        result.put( name, configuration );
      }
    }

    return result;
  }
}
