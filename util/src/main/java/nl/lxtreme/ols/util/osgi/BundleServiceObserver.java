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
package nl.lxtreme.ols.util.osgi;


import java.util.*;
import java.util.logging.*;

import org.osgi.framework.*;


/**
 * Provides a bundle observer that registers all found manifest entries as
 * services.
 */
public class BundleServiceObserver extends AbstractBundleObserver
{
  // CONSTANTS

  private final Logger LOG = Logger.getLogger( BundleServiceObserver.class.getName() );

  // VARIABLES

  private final String serviceKey;
  private final String serviceClassName;

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

    this.serviceKey = aServiceKey;
    this.serviceClassName = aServiceClassName;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.util.osgi.AbstractBundleObserver#doAdded(org.osgi.framework.Bundle,
   *      nl.lxtreme.ols.util.osgi.ManifestHeader[])
   */
  @Override
  protected void doAdded( final Bundle aBundle, final ManifestHeader... aEntries )
  {
    final String[] values = getManifestHeaderValues( this.serviceKey, aEntries );
    if ( values != null )
    {
      final BundleContext bundleContext = aBundle.getBundleContext();

      for ( String className : values )
      {
        try
        {
          final Class<?> clazz = aBundle.loadClass( className );

          final Object newService = clazz.newInstance();

          bundleContext
              .registerService( this.serviceClassName, newService, getServiceProperties( aBundle, newService ) );

          this.LOG.log( Level.INFO, "New service (" + className + ") registered ..." );
        }
        catch ( ClassNotFoundException exception )
        {
          this.LOG.log( Level.WARNING, "Service class not found: " + className + "! Is it included in the bundle?" );
        }
        catch ( InstantiationException exception )
        {
          this.LOG.log( Level.WARNING, "Service (" + className
              + ") could not be instantiated! Is should be a public concrete class (not abstract, nor an interface!)" );
        }
        catch ( IllegalAccessException exception )
        {
          this.LOG.log( Level.WARNING, "Service (" + className
              + ") could not be instantiated! Is the class public and does it have a public default constructor?" );
        }
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
    final String[] values = getManifestHeaderValues( this.serviceKey, aEntries );
    if ( values != null )
    {
      for ( String className : values )
      {
        this.LOG.log( Level.INFO, "Service (" + className + ") unregistered ..." );
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
  protected Dictionary<?, ?> getServiceProperties( final Bundle aBundle, final Object aService )
  {
    return null;
  }
}
