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
package nl.lxtreme.ols.client.ui.util;


import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.*;

import nl.lxtreme.ols.common.Configuration;
import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.metatype.*;


/**
 * Provides a base class for service wrappers.
 */
public abstract class DelegateServiceWrapper<SERVICE>
{
  // INNER TYPES

  /**
   * Wraps a given {@link ConfigurationListener} to be called only for a
   * specified PID. When the event is received, the component is unregistered.
   */
  protected static final class ConfigurationListenerWrapper implements ConfigurationListener
  {
    // VARIABLES

    private final ConfigurationListener delegate;
    private final String pid;
    // Injected by Felix DM...
    private volatile DependencyManager dependencyManager;
    private volatile Component component;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ConfigurationListenerWrapper} instance.
     */
    public ConfigurationListenerWrapper( final ConfigurationListener aDelegate, final String aPID )
    {
      this.delegate = aDelegate;
      this.pid = aPID;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void configurationEvent( final ConfigurationEvent aEvent )
    {
      if ( this.pid.equals( aEvent.getPid() ) )
      {
        this.delegate.configurationEvent( aEvent );

        // As we've got what we wanted, remove the component from our service
        // registry...
        this.dependencyManager.remove( this.component );
      }
    }
  }

  /**
   * Provides a thread-safe version of {@link Configuration}.
   */
  protected static final class MutableConfiguration implements Configuration
  {
    // VARIABLES

    private final AtomicReference<Map<Object, Object>> mapRef;

    // CONSTRUCTORS

    /**
     * Creates a new {@link MutableConfiguration} instance.
     */
    public MutableConfiguration()
    {
      this.mapRef = new AtomicReference<Map<Object, Object>>( new HashMap<Object, Object>() );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<Object, Object> asMap()
    {
      return this.mapRef.get();
    }

    /**
     * @return <code>true</code> if this configuration is empty,
     *         <code>false</code> otherwise.
     */
    public boolean isEmpty()
    {
      return this.mapRef.get().isEmpty();
    }

    /**
     * Sets the configuration of this object to the given value.
     * 
     * @param aValue
     *          the new configuration to set, cannot be <code>null</code>.
     */
    @SuppressWarnings( "rawtypes" )
    public void set( final Dictionary aValue )
    {
      Map<Object, Object> value = asMap( aValue );
      Map<Object, Object> old;
      do
      {
        old = this.mapRef.get();
      }
      while ( !this.mapRef.compareAndSet( old, value ) );
    }

    /**
     * Converts a given {@link Dictionary} to a {@link Map}.
     * 
     * @param aValue
     *          the dictionary to convert, can be <code>null</code>.
     * @return a map representation of the given {@link Dictionary}, or an empty
     *         map is the given value was <code>null</code>.
     */
    @SuppressWarnings( "rawtypes" )
    private Map<Object, Object> asMap( final Dictionary aValue )
    {
      HashMap<Object, Object> result = new HashMap<Object, Object>();
      if ( aValue != null )
      {
        Enumeration keys = aValue.keys();
        while ( keys.hasMoreElements() )
        {
          Object key = keys.nextElement();
          result.put( key, aValue.get( key ) );
        }
      }
      return result;
    }
  }

  // VARIABLES

  private final SERVICE delegate;
  private final MutableConfiguration configuration;
  // Injected by Felix DM...
  private volatile BundleContext bundleContext;
  private volatile MetaTypeService metaTypeService;
  private volatile ConfigurationAdmin configAdmin;

  // CONSTRUCTOR

  /**
   * Creates a new DelegateServiceWrapper instance.
   */
  public DelegateServiceWrapper( final SERVICE aDelegate )
  {
    this.delegate = aDelegate;
    this.configuration = new MutableConfiguration();
  }

  // METHODS

  /**
   * Returns the current value of delegate.
   * 
   * @return the delegate service, never <code>null</code>.
   */
  protected final SERVICE getDelegate()
  {
    return this.delegate;
  }

  /**
   * Returns the current value of configuration.
   * 
   * @return the configuration
   */
  protected final MutableConfiguration getConfiguration()
  {
    return this.configuration;
  }

  /**
   * @return a {@link MetaTypeInformation} instance, can be <code>null</code> if
   *         the bundle has no metatype information.
   */
  protected final MetaTypeInformation getMetaTypeInfo()
  {
    return this.metaTypeService.getMetaTypeInformation( this.bundleContext.getBundle() );
  }

  /**
   * @param aLocale
   *          the locale to use for the {@link ObjectClassDefinition}.
   * @return a {@link ObjectClassDefinition} instance, can be <code>null</code>.
   */
  protected final ObjectClassDefinition getOCD( final Locale aLocale )
  {
    MetaTypeInformation metaTypeInfo = getMetaTypeInfo();
    if ( ( metaTypeInfo == null ) || ( metaTypeInfo.getPids().length != 1 ) )
    {
      // Not metatyped; assume it has no configuration to be performed...
      return null;
    }

    String pid = metaTypeInfo.getPids()[0];

    return metaTypeInfo.getObjectClassDefinition( pid, aLocale.toString() );
  }

  /**
   * Called by Felix DM upon initialization of this component.
   */
  protected final void init( final Component aComponent )
  {
    MetaTypeInformation metaTypeInfo = getMetaTypeInfo();
    if ( ( metaTypeInfo != null ) && ( metaTypeInfo.getPids().length == 1 ) )
    {
      Dictionary<Object, Object> dict = new Hashtable<Object, Object>();
      dict.put( org.osgi.framework.Constants.SERVICE_PID, metaTypeInfo.getPids()[0] );

      aComponent.setServiceProperties( dict );
    }
  }

  /**
   * @param aPid
   * @param aProperties
   */
  protected final void updateConfiguration( final String aPid, final Dictionary<Object, Object> aProperties )
      throws IOException
  {
    org.osgi.service.cm.Configuration config = this.configAdmin.getConfiguration( aPid );
    config.update( aProperties );

    getConfiguration().set( aProperties );
  }
}
