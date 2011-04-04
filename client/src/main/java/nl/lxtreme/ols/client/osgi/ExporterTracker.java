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

import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * Provides an OSGi service tracker for exporters.
 */
public class ExporterTracker extends ServiceTracker
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( ExporterTracker.class.getName() );

  // VARIABLES

  private final ClientController controller;

  // CONSTRUCTORS

  /**
   * @param aContext
   *          the bundle context to use;
   * @param aController
   *          the client controller to use.
   */
  public ExporterTracker( final BundleContext aContext, final ClientController aController )
  {
    super( aContext, Exporter.class.getName(), null );
    this.controller = aController;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    Exporter exporter = null;

    try
    {
      exporter = ( Exporter )this.context.getService( aReference );
      this.controller.addExporter( exporter );
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Exporer service not added! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }

    return exporter;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    try
    {
      final Exporter exporter = ( Exporter )aService;

      super.removedService( aReference, exporter );

      this.controller.removeExporter( exporter );
    }
    catch ( Exception exception )
    {
      LOG.log( Level.WARNING, "Exporter service not removed! Reason: {0}", exception.getMessage() );
      LOG.log( Level.FINE, "Details:", exception );
    }
  }
}
