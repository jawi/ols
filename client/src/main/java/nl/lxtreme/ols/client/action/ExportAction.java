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
package nl.lxtreme.ols.client.action;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.logging.*;

import javax.swing.*;
import javax.swing.filechooser.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a export to file functionality.
 */
public class ExportAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "ExportAction.";

  private static final Logger LOG = Logger.getLogger( ExportAction.class.getName() );

  // VARIABLES

  private final String exporterName;

  // CONSTRUCTORS

  /**
   * Creates a new ExportAction instance.
   *
   * @param aController
   *          the client controller to use;
   * @param aExporterName
   *          the name of the exporter to invoke in this action.
   */
  public ExportAction( final ClientController aController, final String aExporterName )
  {
    super( ID + aExporterName, aController, aExporterName, "Export the current diagram to a " + aExporterName
        + " file" );

    this.exporterName = aExporterName;
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    final ClientController controller = getController();
    final DataContainer dataContainer = controller.getDataContainer();

    if ( !dataContainer.hasCapturedData() )
    {
      JOptionPane.showMessageDialog( owner, "Nothing to export!", "Error", JOptionPane.ERROR_MESSAGE );
      return;
    }

    final Exporter exporter = controller.getExporter( this.exporterName );

    final File exportFileName = getExportFileName( owner, exporter );
    if ( exportFileName != null )
    {
      OutputStream writer = null;

      try
      {
        writer = new FileOutputStream( exportFileName );

        controller.exportTo( exporter, writer );

        controller.setStatus( "Export to {0} succesful ...", this.exporterName );
      }
      catch ( IOException exception )
      {
        controller.setStatus( "Export to {0} failed! Reason: " + exception.getMessage(), this.exporterName );
      }
      finally
      {
        try
        {
          writer.flush();
        }
        catch ( IOException exception )
        {
          LOG.log( Level.FINE, "Flushing (file) writer failed?!", exception );
        }

        try
        {
          writer.close();
          writer = null;
        }
        catch ( IOException exception )
        {
          LOG.log( Level.FINE, "Closing (file) writer failed?!", exception );
        }
      }
    }
  }

  /**
   * Returns the exporter file name.
   *
   * @param aOwner
   *          the parent window;
   * @param aExporter
   *          the export to use.
   * @return a export filename, can be <code>null</code> in case no file is
   *         selected by the user.
   */
  private File getExportFileName( final Window aOwner, final Exporter aExporter )
  {
    final FileNameExtensionFilter fileFilter = new FileNameExtensionFilter( "Valid export format(s)",
        aExporter.getFilenameExtentions() );

    final File exportFileName = SwingComponentUtils.showFileSaveDialog( aOwner, fileFilter );
    return exportFileName;
  }
}
