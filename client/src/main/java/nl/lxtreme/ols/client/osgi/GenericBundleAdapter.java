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
package nl.lxtreme.ols.client.osgi;


import java.util.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * Provides a bundle adapter that instantiates a service from a bundle with a
 * particular magic key-value header.
 */
public class GenericBundleAdapter<TYPE>
{
  // VARIABLES

  private final Class<? extends TYPE> serviceClass;
  private final String headerKey;

  private volatile Bundle bundle; // = adapted bundle
  private volatile DependencyManager manager; // injected
  private volatile Component serviceComponent; // = added service

  // CONSTRUCTORS

  /**
   * Creates a new GenericBundleAdapter instance.
   */
  public GenericBundleAdapter( final Class<? extends TYPE> aClass, final String aHeaderKey )
  {
    this.serviceClass = aClass;
    this.headerKey = aHeaderKey;
  }

  // METHODS

  /**
   * Called by Dependency Manager upon destruction of this component.
   */
  public void destroy( final Component aComponent ) throws Exception
  {
    if ( this.serviceComponent != null )
    {
      this.manager.remove( this.serviceComponent );
    }
  }

  /**
   * Called by Dependency Manager upon initialization of this component.
   */
  public void init( final Component aComponent ) throws Exception
  {
    Dictionary<?, ?> bundleProps = this.bundle.getHeaders();

    String entry = ( String )bundleProps.get( this.headerKey );
    if ( ( entry == null ) || "".equals( entry ) )
    {
      throw new IllegalArgumentException( "No such header given: " + this.headerKey );
    }

    Class<?> implClass = this.bundle.loadClass( entry );

    Properties serviceProps = copyOlsProperties( bundleProps );

    this.serviceComponent = this.manager.createComponent() //
        .setInterface( this.serviceClass.getName(), serviceProps ) //
        .setImplementation( implClass ) //
        .add( this.manager.createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) //
        );
    this.manager.add( this.serviceComponent );
  }

  /**
   * Copies all OLS-specific properties from the original bundle header to a new
   * {@link Properties} instance.
   * 
   * @param aOriginal
   *          the original bundle header to copy, cannot be <code>null</code>.
   * @return a {@link Properties} instance with the OLS-specific properties,
   *         never <code>null</code>.
   */
  private Properties copyOlsProperties( final Dictionary<?, ?> aOriginal )
  {
    Properties result = new Properties();

    final Enumeration<?> keys = aOriginal.keys();
    while ( keys.hasMoreElements() )
    {
      final Object key = keys.nextElement();
      if ( key.toString().startsWith( "OLS-" ) )
      {
        result.put( key, aOriginal.get( key ) );
      }
    }

    return result;
  }
}
