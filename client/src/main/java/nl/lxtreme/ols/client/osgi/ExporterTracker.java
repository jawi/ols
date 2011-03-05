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


import javax.swing.*;

import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * @author jawi
 */
public class ExporterTracker extends ServiceTracker
{
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
    final Exporter exporter = ( Exporter )this.context.getService( aReference );

    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        ExporterTracker.this.controller.addExporter( exporter );
      }
    } );

    return exporter;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    final Exporter exporter = ( Exporter )aService;

    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        ExporterTracker.this.controller.removeExporter( exporter );
      }
    } );

    super.removedService( aReference, aService );
  }
}
