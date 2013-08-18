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

import javax.swing.filechooser.*;
import javax.swing.filechooser.FileFilter;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides an "open data file" action.
 */
public class OpenDataFileAction extends BaseAction
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( OpenDataFileAction.class.getName() );

  private static final long serialVersionUID = 1L;

  public static final String ID = "OpenDataFile";

  public static final String OLS_FILE_EXTENSION = "ols";
  public static final FileFilter OLS_FILEFILTER = new FileNameExtensionFilter( "OpenLogic Sniffer data file",
      OLS_FILE_EXTENSION );

  // CONSTRUCTORS

  /**
   * Creates a new OpenDataFileAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public OpenDataFileAction( final ClientController aController )
  {
    super( ID, aController, ICON_OPEN_DATAFILE, "Open ...", "Open an existing data file" );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_O ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    final File file = SwingComponentUtils.showFileOpenDialog( owner, OLS_FILEFILTER );
    if ( file != null )
    {
      LOG.log( Level.INFO, "Loading capture data from file {0}", file );

      try
      {
        getController().openDataFile( file );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          LOG.log( Level.WARNING, "Loading OLS file failed!", exception );
          JErrorDialog.showDialog( owner, "Loading the capture data failed!", exception );
        }
      }
    }
  }
}

/* EOF */
