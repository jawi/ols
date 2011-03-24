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


import java.util.logging.*;

import nl.lxtreme.ols.api.ui.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * Provides a tracker for generic menu items.
 */
public class MenuTracker extends ServiceTracker
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( MenuTracker.class.getName() );

  // VARIABLES

  private final ClientController controller;

  // CONSTRUCTORS

  /**
   * Creates a new MenuTracker instance.
   * 
   * @param aContext
   * @param aMenuBar
   */
  public MenuTracker( final BundleContext aContext, final ClientController aController )
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
    if ( !ComponentProvider.MENU_COMPONENT.equals( aReference.getProperty( ComponentProvider.COMPONENT_ID_KEY ) ) )
    {
      return null;
    }

    ComponentProvider provider = null;

    try
    {
      provider = ( ComponentProvider )this.context.getService( aReference );

      this.controller.addMenu( provider );
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
    if ( !ComponentProvider.MENU_COMPONENT.equals( aReference.getProperty( ComponentProvider.COMPONENT_ID_KEY ) ) )
    {
      return;
    }

    try
    {
      final ComponentProvider provider = ( ComponentProvider )aService;

      super.removedService( aReference, provider );

      this.controller.removeMenu( provider );
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Component provider service not removed! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }
  }
}
