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
package nl.lxtreme.ols.util.osgi;


import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;


/**
 * Provides a bundle observer that registers all found manifest entries as
 * services.
 */
public class BundleServiceObserver extends AbstractBundleObserver
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( BundleServiceObserver.class.getName() );

  // VARIABLES

  private final String serviceKey;
  private final String serviceClassName;

  private final ConcurrentMap<Long, List<Pair<ServiceRegistration, Object>>> registry;

  // CONSTRUCTROS

  /**
   * Creates a new BundleServiceObserver instance.
   * 
   * @param aMagicKey
   *          the "magic" key to search for;
   * @param aMagicValue
   *          the "magic" value to search for;
   * @param aServiceKey
   *          the service key to search for;
   * @param aServiceClassName
   *          the class name of the service to register new service under.
   */
  public BundleServiceObserver( final String aMagicKey, final String aMagicValue, final String aServiceKey,
      final String aServiceClassName )
  {
    super( aMagicKey, aMagicValue );

    this.registry = new ConcurrentHashMap<Long, List<Pair<ServiceRegistration, Object>>>();

    this.serviceKey = aServiceKey;
    this.serviceClassName = aServiceClassName;
  }

  // METHODS

  /**
   * Destroys the given service by searching for an <tt>destroy()</tt> method,
   * and if found, invokes this method. If the destroy-method is not found, this
   * method will not do anything.
   * 
   * @param aService
   *          the service to destroy.
   */
  protected final void destroyService( final Object aService )
  {
    final Class<?> serviceClazz = aService.getClass();
    try
    {
      final Method initMethod = serviceClazz.getDeclaredMethod( "destroy" );
      if ( initMethod != null )
      {
        initMethod.setAccessible( true );
        initMethod.invoke( aService );
      }
    }
    catch ( SecurityException exception )
    {
      LOG.log( Level.INFO, "Security exception while trying to destroy service...", exception );
    }
    catch ( NoSuchMethodException exception )
    {
      LOG.log( Level.FINE, "No destory-method found!" );
    }
    catch ( IllegalArgumentException exception )
    {
      LOG.log( Level.FINE, "Illegal argument while trying to destroy service!", exception );
    }
    catch ( IllegalAccessException exception )
    {
      LOG.log( Level.FINE, "Illegal access to destroy-method!", exception );
    }
    catch ( InvocationTargetException exception )
    {
      LOG.log( Level.WARNING, "Service destruction failed!", exception.getCause() );
    }
  }

  /**
   * @see nl.lxtreme.ols.util.osgi.AbstractBundleObserver#doAdded(org.osgi.framework.Bundle,
   *      nl.lxtreme.ols.util.osgi.ManifestHeader[])
   */
  @Override
  protected void doAdded( final Bundle aBundle, final ManifestHeader... aEntries )
  {
    final String[] values = getManifestHeaderValues( this.serviceKey, aEntries );
    if ( values.length == 0 )
    {
      return;
    }

    List<Pair<ServiceRegistration, Object>> pairs = this.registry.get( Long.valueOf( aBundle.getBundleId() ) );
    if ( pairs == null )
    {
      pairs = new CopyOnWriteArrayList<Pair<ServiceRegistration, Object>>();
      this.registry.put( Long.valueOf( aBundle.getBundleId() ), pairs );
    }

    final BundleContext bundleContext = aBundle.getBundleContext();
    for ( String className : values )
    {
      try
      {
        final Class<?> clazz = aBundle.loadClass( className );

        final Object newService = clazz.newInstance();

        final ServiceRegistration serviceRegistration = bundleContext.registerService( this.serviceClassName,
            newService, getServiceProperties( aBundle, newService, aEntries ) );

        pairs.add( Pair.of( serviceRegistration, newService ) );

        // Give the just registered service to do additional tasks as well...
        initializeService( newService, bundleContext );

        LOG.log( Level.INFO, "New service (" + className + ") registered ..." );
      }
      catch ( ClassNotFoundException exception )
      {
        LOG.log( Level.WARNING, "Service class not found: " + className + "! Is it included in the bundle?" );
      }
      catch ( InstantiationException exception )
      {
        LOG.log( Level.WARNING, "Service (" + className
            + ") could not be instantiated! Is should be a public concrete class (not abstract, nor an interface!)" );
      }
      catch ( IllegalAccessException exception )
      {
        LOG.log( Level.WARNING, "Service (" + className
            + ") could not be instantiated! Is the class public and does it have a public default constructor?" );
      }
    }
  }

  /**
   * @see nl.lxtreme.ols.util.osgi.AbstractBundleObserver#doRemoved(org.osgi.framework.Bundle,
   *      nl.lxtreme.ols.util.osgi.ManifestHeader[])
   */
  @Override
  protected void doRemoved( final Bundle aBundle, final ManifestHeader... aEntries )
  {
    List<Pair<ServiceRegistration, Object>> pairs = this.registry.remove( Long.valueOf( aBundle.getBundleId() ) );
    if ( pairs != null )
    {
      for ( Pair<ServiceRegistration, Object> pair : pairs )
      {
        final Object service = pair.getRight();

        destroyService( service );

        try
        {
          pair.getLeft().unregister();

          LOG.log( Level.INFO, "Service (" + service.getClass().getName() + ") unregistered ..." );
        }
        catch ( IllegalStateException exception )
        {
          LOG.log( Level.FINE, "Service (" + service.getClass().getName() + ") was already unregistered!" );
        }
      }
    }
  }

  /**
   * Returns the (optional) service properties that should be registered along
   * with the new service instance.
   * 
   * @param aBundle
   *          the bundle registering the service;
   * @param aService
   *          the service to register.
   * @return the service properties, may be <code>null</code>.
   */
  protected Dictionary<?, ?> getServiceProperties( final Bundle aBundle, final Object aService,
      final ManifestHeader... aEntries )
  {
    return null;
  }

  /**
   * Initializes the given service by searching for an
   * <tt>init(BundleContext)</tt> method, and if found, invokes this method. If
   * the init method is not found, this method will not do anything.
   * 
   * @param aService
   *          the service to initialize;
   * @param aBundleContext
   *          the bundle context to initialize the service with.
   */
  protected final void initializeService( final Object aService, final BundleContext aBundleContext )
  {
    final Class<?> serviceClazz = aService.getClass();
    try
    {
      final Method initMethod = serviceClazz.getDeclaredMethod( "init", BundleContext.class );
      if ( initMethod != null )
      {
        initMethod.setAccessible( true );
        initMethod.invoke( aService, aBundleContext );
      }
    }
    catch ( SecurityException exception )
    {
      LOG.log( Level.INFO, "Security exception while initializing service...", exception );
    }
    catch ( NoSuchMethodException exception )
    {
      LOG.log( Level.FINE, "No init-method found!" );
    }
    catch ( IllegalArgumentException exception )
    {
      LOG.log( Level.FINE, "Illegal argument while initializing service!", exception );
    }
    catch ( IllegalAccessException exception )
    {
      LOG.log( Level.FINE, "Illegal access to init-method!", exception );
    }
    catch ( InvocationTargetException exception )
    {
      LOG.log( Level.WARNING, "Service initialization failed!", exception.getCause() );
    }
  }
}
