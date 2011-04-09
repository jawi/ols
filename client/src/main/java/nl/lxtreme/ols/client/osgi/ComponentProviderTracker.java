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
package nl.lxtreme.ols.client.osgi;


import java.util.logging.*;

import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * Provides a tracker for generic component providers.
 */
public class ComponentProviderTracker extends ServiceTracker
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ComponentProviderTracker.class.getName() );

  // VARIABLES

  private final ClientController controller;

  // CONSTRUCTORS

  /**
   * Creates a new ComponentProviderTracker instance.
   * 
   * @param aContext
   *          the OSGi bundle context to use;
   * @param aController
   *          the client controller to use.
   */
  public ComponentProviderTracker( final BundleContext aContext, final ClientController aController )
  {
    super( aContext, ComponentProvider.class.getName(), null );
    this.controller = aController;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final Object componentId = aReference.getProperty( ComponentProvider.COMPONENT_ID_KEY );

    ComponentProvider provider = null;

    try
    {
      provider = ( ComponentProvider )this.context.getService( aReference );

      if ( ComponentProvider.MENU_COMPONENT.equals( componentId ) )
      {
        this.controller.addMenu( provider );
      }
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Component provider service not added! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }

    return provider;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    final Object componentId = aReference.getProperty( ComponentProvider.COMPONENT_ID_KEY );

    try
    {
      final ComponentProvider provider = ( ComponentProvider )aService;

      if ( ComponentProvider.MENU_COMPONENT.equals( componentId ) )
      {
        this.controller.removeMenu( provider );
      }
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Component provider service not removed! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }
    finally
    {
      try
      {
        this.context.ungetService( aReference );
      }
      catch ( IllegalStateException exception )
      {
        LOG.log( Level.WARNING, "Failed to unget component provider service! Reason: {0}", exception.getMessage() );
      }
    }
  }
}
