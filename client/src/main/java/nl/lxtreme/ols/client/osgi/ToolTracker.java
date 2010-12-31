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

import nl.lxtreme.ols.api.tools.*;
import nl.lxtreme.ols.client.*;

import org.osgi.framework.*;
import org.osgi.util.tracker.*;


/**
 * 
 */
public class ToolTracker extends ServiceTracker
{
  // VARIABLES

  private final ClientController controller;

  // CONSTRUCTORS

  /**
   * @param aContext
   * @param aReference
   * @param aCustomizer
   */
  public ToolTracker( final BundleContext aContext, final ClientController aController )
  {
    super( aContext, Tool.class.getName(), null );

    this.controller = aController;
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final Tool tool = ( Tool )this.context.getService( aReference );
    registerTool( tool );
    return tool;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    final Tool tool = ( Tool )aService;
    unregisterTool( tool );
  }

  /**
   * @param aTool
   */
  private void registerTool( final Tool aTool )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      public void run()
      {
        ToolTracker.this.controller.addTool( aTool );
      }
    } );
  }

  /**
   * @param aTool
   */
  private void unregisterTool( final Tool aTool )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      public void run()
      {
        ToolTracker.this.controller.removeTool( aTool );
      }
    } );
  }
}

/* EOF */
