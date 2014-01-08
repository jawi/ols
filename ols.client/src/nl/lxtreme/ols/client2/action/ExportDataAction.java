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
package nl.lxtreme.ols.client2.action;


import java.awt.event.*;
import java.io.*;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.filechooser.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.export.api.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a export to file functionality.
 */
public class ExportDataAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "ExportAction.";

  private static final Logger LOG = Logger.getLogger( ExportDataAction.class.getName() );

  // VARIABLES

  private final Exporter exporter;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ExportDataAction} instance.
   * 
   * @param aExporter
   *          the exporter to invoke in this action.
   */
  public ExportDataAction( Exporter aExporter )
  {
    super( getID( aExporter ) );

    this.exporter = aExporter;

    String exporterName = aExporter.getName();

    putValue( NAME, exporterName );
    putValue( SHORT_DESCRIPTION, "Export the current diagram to a " + exporterName + " file" );
    putValue( MENU_NAME, ClientConstants.EXPORT_MENU );
  }

  // METHODS

  /**
   * Creates an ID for an action that represents the "export" action for the
   * exporter with the given name.
   * 
   * @param aExporterName
   *          the name of the exporter to create the ID for, cannot be
   *          <code>null</code>.
   * @return a ID, never <code>null</code>.
   */
  public static final String getID( Exporter aExporter )
  {
    return ID.concat( aExporter.getName() );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );
    String exporterName = this.exporter.getName();

    if ( !client.hasAcquiredData() )
    {
      JOptionPane.showMessageDialog( client, "No data to export!", "Error", JOptionPane.ERROR_MESSAGE );
      return;
    }

    String[] extensions = this.exporter.getFilenameExtentions();
    String preferredExtension = ( extensions.length == 0 ) ? "" : extensions[0];

    File exportFileName = SwingComponentUtils.showFileSaveDialog( client, new FileNameExtensionFilter(
        "Valid export format(s)", extensions ) );
    if ( exportFileName == null )
    {
      return;
    }

    File actualFile = SwingComponentUtils.setFileExtension( exportFileName, preferredExtension );

    if ( LOG.isLoggable( Level.INFO ) )
    {
      LOG.info( "Exporting data to file: " + actualFile.getName() + " using " + exporterName );
    }

    OutputStream os = null;

    try
    {
      os = new FileOutputStream( actualFile );

      this.exporter.export( client.getAcquiredData(), client.getDockingManager().getMainContainer(), os );

      client.setStatus( "Export to %s succesful ...", exporterName );
    }
    catch ( IOException exception )
    {
      LOG.log( Level.WARNING, "Export to " + exporterName + " failed!", exception );

      JErrorDialog.showDialog( client, "Export data to " + exporterName + " failed!", exception );

      client.setStatus( "Export to %s failed ...", exporterName );
    }
    finally
    {
      try
      {
        if ( os != null )
        {
          os.close();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
      }

      client.updateManagedState();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() );
  }
}
