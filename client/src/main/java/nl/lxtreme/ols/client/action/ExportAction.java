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
import java.util.logging.*;

import javax.swing.*;
import javax.swing.filechooser.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


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
    super( getID( aExporterName ), aController, aExporterName, "Export the current diagram to a " + aExporterName
        + " file" );

    this.exporterName = aExporterName;
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

    final ClientController controller = getController();

    if ( !controller.hasCapturedData() )
    {
      JOptionPane.showMessageDialog( owner, "Nothing to export!", "Error", JOptionPane.ERROR_MESSAGE );
      return;
    }

    final String[] extensions = controller.getExportExtensions( this.exporterName );
    final String preferredExtension = ( extensions.length == 0 ) ? "" : extensions[0];

    final File exportFileName = SwingComponentUtils.showFileSaveDialog( owner, //
        new FileNameExtensionFilter( "Valid export format(s)", extensions ) );

    if ( exportFileName != null )
    {
      final File actualFile = HostUtils.setFileExtension( exportFileName, preferredExtension );
      if ( LOG.isLoggable( Level.INFO ) )
      {
        LOG.info( "Exporting capture data to file: " + actualFile );
      }

      try
      {
        controller.exportTo( this.exporterName, actualFile );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          LOG.log( Level.WARNING, "Export with '" + this.exporterName + "' failed!", exception );
          JErrorDialog.showDialog( owner, "Export capture data failed!", exception );
        }
      }
    }
  }
}
