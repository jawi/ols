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

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a "save data file" action.
 */
public class SaveDataFileAction extends BaseAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( SaveDataFileAction.class.getName() );

  public static final String ID = "SaveDataFile";

  // CONSTRUCTORS

  /**
   * Creates a new SaveDataFileAction instance.
   * 
   * @param aController
   *          the controller to use for this action.
   */
  public SaveDataFileAction( final ClientController aController )
  {
    super( ID, aController, ICON_SAVE_DATAFILE, "Save ...", "Save data file" );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_S ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    final File file = SwingComponentUtils.showFileSaveDialog( owner, OpenDataFileAction.OLS_FILEFILTER );
    if ( file != null )
    {
      final File actualFile = HostUtils.setFileExtension( file, OpenDataFileAction.OLS_FILE_EXTENSION );

      LOG.log( Level.INFO, "Saving capture data to file {0}", actualFile );

      try
      {
        getController().saveDataFile( actualFile );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !HostUtils.handleInterruptedException( exception ) )
        {
          LOG.log( Level.WARNING, "Saving capture data failed!", exception );
          JErrorDialog.showDialog( owner, "Saving the capture data failed!", exception );
        }
      }
    }
  }
}

/* EOF */
