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


import java.util.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * Provides a bundle adapter that instantiates a service from a bundle with a
 * particular magic key-value header.
 */
public abstract class AbstractBundleAdapter<TYPE>
{
  // VARIABLES

  protected final Class<? extends TYPE> serviceClass;
  protected final String headerKey;

  protected volatile Bundle bundle; // = adapted bundle
  protected volatile DependencyManager manager; // injected
  protected volatile Component serviceComponent; // = added service

  // CONSTRUCTORS

  /**
   * Creates a new GenericBundleAdapter instance.
   */
  public AbstractBundleAdapter( final Class<? extends TYPE> aClass, final String aHeaderKey )
  {
    this.serviceClass = aClass;
    this.headerKey = aHeaderKey;
  }

  // METHODS

  /**
   * Called by Dependency Manager upon destruction of this component.
   */
  public final void destroy( final Component aComponent ) throws Exception
  {
    if ( this.serviceComponent != null )
    {
      this.manager.remove( this.serviceComponent );
      this.serviceComponent = null;
    }
  }

  /**
   * Called by Dependency Manager upon initialization of this component.
   */
  @SuppressWarnings( "unchecked" )
  public final void init( final Component aComponent ) throws Exception
  {
    // Make sure to register services on behalf of the *original* bundle!
    this.manager = new DependencyManager( this.bundle.getBundleContext() );

    Dictionary<?, ?> bundleProps = this.bundle.getHeaders();

    String entry = ( String )bundleProps.get( this.headerKey );
    if ( ( entry == null ) || "".equals( entry ) )
    {
      throw new IllegalArgumentException( "No such header given: " + this.headerKey );
    }

    Class<TYPE> implClass = this.bundle.loadClass( entry );

    Class<?>[] interfaces = getPublishedInterfaces();
    String[] intfNames = new String[interfaces.length];
    for ( int i = 0; i < intfNames.length; i++ )
    {
      intfNames[i] = interfaces[i].getName();
    }

    Properties serviceProps = copyOlsProperties( bundleProps );

    this.serviceComponent = this.manager.createComponent() //
        .setInterface( intfNames, serviceProps ) //
        .setImplementation( getImplementation( implClass ) ) //
        .add( this.manager.createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) //
        );
    addServiceDependencies( this.manager, this.serviceComponent );
    this.manager.add( this.serviceComponent );
  }

  /**
   * Registers additional service dependencies for the given component.
   * 
   * @param aManager
   *          the dependency manager to use;
   * @param aComponent
   *          the component to add service dependencies for.
   */
  protected void addServiceDependencies( final DependencyManager aManager, final Component aComponent )
  {
    // Nothing...
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
  protected final Properties copyOlsProperties( final Dictionary<?, ?> aOriginal )
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

  /**
   * Creates an implementation or returns an implementation=-class for the given
   * type.
   * 
   * @param aType
   *          the type to create an service implementation for, cannot be
   *          <code>null</code>.
   * @return a implementation type or instance, never <code>null</code>.
   * @throws Exception
   *           in case of problems obtaining the implementation.
   */
  protected Object getImplementation( final Class<TYPE> aType ) throws Exception
  {
    return aType;
  }

  /**
   * Returns the interfaces the service class should publish.
   * 
   * @return an array with published interfaces, never <code>null</code>.
   */
  protected Class<?>[] getPublishedInterfaces()
  {
    return new Class<?>[] { this.serviceClass };
  }
}
