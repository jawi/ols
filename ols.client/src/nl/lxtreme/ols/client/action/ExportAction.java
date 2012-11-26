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
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.filechooser.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.export.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.log.*;


/**
 * Provides a export to file functionality.
 */
public class ExportAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "ExportAction.";

  // VARIABLES

  private final ImportExportController exportController;
  private final String exporterName;

  // CONSTRUCTORS

  /**
   * Creates a new ExportAction instance.
   * 
   * @param aImportExportController
   *          the client controller to use;
   * @param aExporterName
   *          the name of the exporter to invoke in this action.
   */
  public ExportAction( final ImportExportController aImportExportController, final String aExporterName )
  {
    this.exportController = aImportExportController;
    this.exporterName = aExporterName;

    putValue( NAME, aExporterName );
    putValue( SHORT_DESCRIPTION, "Export the current diagram to a " + aExporterName + " file" );
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
  public static final String getID( final String aExporterName )
  {
    return ID.concat( aExporterName );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    if ( !hasCapturedData() )
    {
      JOptionPane.showMessageDialog( owner, "Nothing to export!", "Error", JOptionPane.ERROR_MESSAGE );
      return;
    }

    final String[] extensions = this.exportController.getExportExtensions( this.exporterName );
    final String preferredExtension = ( extensions.length == 0 ) ? "" : extensions[0];

    final File exportFileName = SwingComponentUtils.showFileSaveDialog( owner, //
        new FileNameExtensionFilter( "Valid export format(s)", extensions ) );

    if ( exportFileName != null )
    {
      final File actualFile = FileExtensionUtils.setFileExtension( exportFileName, preferredExtension );

      try
      {
        exportTo( this.exporterName, actualFile );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !IOUtil.handleInterruptedException( exception ) )
        {
          JErrorDialog.showDialog( owner, "Export capture data failed!", exception );
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return getID( this.exporterName );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState()
  {
    setEnabled( hasCapturedData() );
  }

  /**
   * Exports the current data set to a file using an {@link Exporter} with a
   * given name.
   * 
   * @param aExporterName
   *          the name of the exporter to use, cannot be <code>null</code>;
   * @param aExportFile
   *          the file to export the results to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the export.
   */
  private void exportTo( final String aExporterName, final File aExportFile ) throws IOException
  {
    final Client client = Client.getInstance();

    Session session = client.getSession();
    if ( !session.hasData() )
    {
      client.getLogService().log( LogService.LOG_INFO, "No data to export, not doing anything..." );
      return;
    }

    OutputStream writer = null;

    try
    {
      writer = new FileOutputStream( aExportFile );

      final SignalDiagramController signalDiagramController = Client.getInstance().getSignalDiagramController();

      final Exporter exporter = this.exportController.getExporter( aExporterName );
      exporter.export( session.getAcquisitionData(), signalDiagramController.getSignalDiagram(), writer );

      client.setStatus( "Export to {0} succesful ...", aExporterName );
    }
    finally
    {
      IOUtil.closeResource( writer );
    }
  }

  /**
   * @return <code>true</code> if there is data captured to export,
   *         <code>false</code> otherwise.
   */
  private boolean hasCapturedData()
  {
    final Session session = Client.getInstance().getSession();
    // Session can only be null in cases where the client is starting up or
    // shutting down...
    return ( session != null ) && session.hasData();
  }
}
