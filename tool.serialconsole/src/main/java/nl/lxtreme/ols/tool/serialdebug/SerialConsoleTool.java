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
package nl.lxtreme.ols.tool.serialdebug;


import java.awt.*;

import javax.swing.*;

import org.apache.felix.dm.*;
import org.apache.felix.dm.Component;
import org.osgi.service.io.*;

import nl.lxtreme.ols.api.data.annotation.*;
import nl.lxtreme.ols.api.tools.*;


/**
 * Provides a serial console, much like Hercules SETUP Utility.
 */
public class SerialConsoleTool implements Tool<Void>
{
  // VARIABLES

  private volatile ConnectorService connectorService;
  private volatile SerialConsoleWindow consoleWindow;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolTask<Void> createToolTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ToolCategory getCategory()
  {
    return ToolCategory.OTHER;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Serial console ...";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void invoke( final Window aParent, final ToolContext aContext )
  {
    disposeConsoleWindow();

    this.consoleWindow = new SerialConsoleWindow( aParent )
    {
      private static final long serialVersionUID = 1L;

      @Override
      protected ConnectorService getConnectorService()
      {
        return SerialConsoleTool.this.connectorService;
      }
    };
    this.consoleWindow.showDialog();
  }

  /**
   * Called when this class is unregistered as OSGi service.
   * 
   * @param aComponent
   *          the bundle context to use, cannot be <code>null</code>.
   */
  protected void destroy( final Component aComponent )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override
      public void run()
      {
        disposeConsoleWindow();
      }
    } );
  }

  /**
   * Called when this class is registered as OSGi service.
   * 
   * @param aComponent
   *          the bundle context to use, cannot be <code>null</code>.
   */
  protected void init( final Component aComponent )
  {
    DependencyManager dependencyManager = aComponent.getDependencyManager();

    aComponent //
        .add( dependencyManager.createServiceDependency() //
            .setService( ConnectorService.class ) //
            .setAutoConfig( "connectorService" ) //
            .setInstanceBound( true ) //
            .setRequired( true ) //
        );
  }

  /**
   * Disposes of the current console window, if any is currently visible.
   */
  private void disposeConsoleWindow()
  {
    if ( this.consoleWindow != null )
    {
      this.consoleWindow.close();
      this.consoleWindow = null;
    }
  }
}
