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
package nl.lxtreme.ols.client;


import javax.swing.*;

import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.client.diagram.*;
import nl.lxtreme.ols.client.diagram.export.*;
import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;


/**
 * 
 */
public class Activator implements BundleActivator
{
  // INNER TYPES

  /**
   *
   */
  static class ImageExporterServiceFactory implements ServiceFactory
  {
    // VARIABLES

    private final Host host;

    // CONSTRUCTORS

    /**
     * 
     */
    public ImageExporterServiceFactory( final Host aHost )
    {
      this.host = aHost;
    }

    // METHODS

    /**
     * @see org.osgi.framework.ServiceFactory#getService(org.osgi.framework.Bundle,
     *      org.osgi.framework.ServiceRegistration)
     */
    @Override
    public Object getService( final Bundle aBundle, final ServiceRegistration aRegistration )
    {
      return createImageExporterService();
    }

    /**
     * @see org.osgi.framework.ServiceFactory#ungetService(org.osgi.framework.Bundle,
     *      org.osgi.framework.ServiceRegistration, java.lang.Object)
     */
    @Override
    public void ungetService( final Bundle aBundle, final ServiceRegistration aRegistration, final Object aService )
    {
      // NO-op
    }

    /**
     * @return
     */
    private ImageExporter createImageExporterService()
    {
      final ClientController controller = this.host.getController();
      final Diagram diagram = controller.getMainFrame().getDiagram();
      final JComponent contentPane = ( JComponent )diagram.getParent().getParent();

      return new ImageExporter( contentPane );
    }
  }

  // VARIABLES

  private Host host = null;

  // METHODS

  /**
   * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext aContext ) throws Exception
  {
    this.host = new Host( aContext );

    // Register the image exporter as service...
    aContext.registerService( Exporter.class.getName(), new ImageExporterServiceFactory( this.host ), null );

    // This has to be done *before* any other Swing related code is executed
    // so this also means the #invokeLater call done below...
    HostUtils.initOSSpecifics( Host.getShortName(), this.host );

    final Runnable initTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.initialize();
      }
    };

    final Runnable startTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.start();
      }
    };

    SwingUtilities.invokeAndWait( initTask );

    SwingUtilities.invokeLater( startTask );
  }

  /**
   * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext aContext ) throws Exception
  {
    final Runnable stopTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.stop();
      }
    };
    SwingUtilities.invokeAndWait( stopTask );

    final Runnable shutdownTask = new Runnable()
    {
      @Override
      public void run()
      {
        Activator.this.host.shutdown();
      }
    };
    SwingUtilities.invokeAndWait( shutdownTask );
  }
}

/* EOF */
