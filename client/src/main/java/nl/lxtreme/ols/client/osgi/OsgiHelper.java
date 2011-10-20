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
package nl.lxtreme.ols.client.osgi;


import java.util.*;

import org.osgi.framework.*;


/**
 * 
 */
public final class OsgiHelper
{
  // VARIABLES

  private final BundleContext bundleContext;

  // CONSTRUCTORS

  /**
   * Creates a new OsgiHelper instance.
   */
  public OsgiHelper( final BundleContext aContext )
  {
    this.bundleContext = aContext;
  }

  // METHODS

  /**
   * Helper method to obtain a certain property from all services of a certain
   * type.
   * 
   * @param aPropertyName
   *          the name of the property to obtain;
   * @param aServiceClass
   *          the service type to obtain the properties for.
   * @return an array with all requested properties, never <code>null</code>.
   */
  public String[] getAllServicePropertiesFor( final String aPropertyName, final Class<?> aServiceClass )
  {
    List<String> result = new ArrayList<String>();

    try
    {
      ServiceReference[] serviceRefs = this.bundleContext.getServiceReferences( aServiceClass.getName(), null );
      if ( serviceRefs != null )
      {
        for ( ServiceReference serviceRef : serviceRefs )
        {
          result.add( ( String )serviceRef.getProperty( aPropertyName ) );
        }
      }
    }
    catch ( InvalidSyntaxException exception )
    {
      throw new RuntimeException( "Invalid filter syntax?!" );
    }

    return result.toArray( new String[result.size()] );
  }

  /**
   * Helper method to obtain a service by its name, as registered in its service
   * properties.
   * <p>
   * For this method to work, the service should be registered under a name with
   * as property the value of <tt>aPropertyName</tt>.
   * </p>
   * 
   * @param aServiceClass
   *          the service type to obtain;
   * @param aPropertyName
   *          the name of the property to filter on;
   * @param aPropertyValue
   *          the value of the property to filter on.
   * @throws RuntimeException
   *           in case the filter is invalid.
   */
  @SuppressWarnings( "unchecked" )
  public <T> T getService( final Class<T> aServiceClass, final String aPropertyName, final String aPropertyValue )
      throws IllegalArgumentException
  {
    if ( ( aPropertyValue == null ) || aPropertyValue.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Name cannot be null or empty!" );
    }

    try
    {
      final ServiceReference[] serviceRefs = this.bundleContext.getServiceReferences( aServiceClass.getName(),
          String.format( "(%s=%s)", aPropertyName, aPropertyValue ) );

      if ( ( serviceRefs != null ) && ( serviceRefs.length > 0 ) )
      {
        return ( T )this.bundleContext.getService( serviceRefs[0] );
      }

      return null;
    }
    catch ( InvalidSyntaxException exception )
    {
      throw new RuntimeException( "getService failed!", exception );
    }
  }

}
