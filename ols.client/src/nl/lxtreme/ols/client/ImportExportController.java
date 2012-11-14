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
package nl.lxtreme.ols.client;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.export.*;

import org.osgi.service.log.*;


/**
 * Provides a front-end controller for accessing import/export functionality.
 */
public final class ImportExportController
{
  // VARIABLES

  private final ConcurrentMap<String, Exporter> exporters;

  // Injected by Felix DM...
  private volatile ActionManager actionManager;
  private volatile LogService logService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ImportExportController} instance.
   */
  public ImportExportController()
  {
    this.exporters = new ConcurrentHashMap<String, Exporter>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  public Exporter getExporter( final String aName )
  {
    if ( aName == null )
    {
      throw new IllegalArgumentException( "Name cannot be null!" );
    }
    return this.exporters.get( aName );
  }

  /**
   * {@inheritDoc}
   */
  public String[] getExporterNames()
  {
    List<String> result = new ArrayList<String>( this.exporters.keySet() );
    // Make sure we've got a predictable order of names...
    Collections.sort( result );

    return result.toArray( new String[result.size()] );
  }

  /**
   * {@inheritDoc}
   */
  public String[] getExportExtensions( final String aExporterName )
  {
    final Exporter exporter = getExporter( aExporterName );
    if ( exporter == null )
    {
      return new String[0];
    }
    return exporter.getFilenameExtentions();
  }

  /**
   * Adds a given exporter to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aExporter
   *          the exporter to add, cannot be <code>null</code>.
   */
  final void addExporter( final Exporter aExporter )
  {
    String name = aExporter.getName();
    if ( this.exporters.putIfAbsent( name, aExporter ) == null )
    {
      this.logService.log( LogService.LOG_INFO, "Added exporter '" + name + "' ..." );

      this.actionManager.add( new ExportAction( this, name ) );
    }
    else
    {
      this.logService.log( LogService.LOG_INFO, "Add of exporter '" + name + "' failed!" );
    }

    this.actionManager.updateActionStates();
  }

  /**
   * Removes a given exporter from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aExporter
   *          the exporter to remove, cannot be <code>null</code>.
   */
  final void removeExporter( final Exporter aExporter )
  {
    String name = aExporter.getName();
    if ( this.exporters.remove( name, aExporter ) )
    {
      this.logService.log( LogService.LOG_INFO, "Removed exporter '" + name + "' ..." );

      this.actionManager.remove( ExportAction.getID( name ) );
    }
    else
    {
      this.logService.log( LogService.LOG_INFO, "Removal of exporter '" + name + "' failed!" );
    }

    this.actionManager.updateActionStates();
  }
}
