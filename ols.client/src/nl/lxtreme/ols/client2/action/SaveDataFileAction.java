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

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a "save data file" action.
 */
public class SaveDataFileAction extends AbstractFileAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( SaveDataFileAction.class.getName() );

  public static final String ID = "SaveDataFile";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SaveDataFileAction} instance.
   */
  public SaveDataFileAction()
  {
    super( ID );

    putValue( NAME, "Save ..." );
    putValue( SHORT_DESCRIPTION, "Save data file" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_SAVE_DATAFILE ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_S ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 5 );

  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    final File file = SwingComponentUtils.showFileSaveDialog( client, OpenDataFileAction.OLS_FILEFILTER );
    if ( file != null )
    {
      File actualFile = setFileExtension( file, OpenDataFileAction.OLS_FILE_EXTENSION );

      LOG.log( Level.INFO, "Saving capture data to file {0}", actualFile );

      try
      {
        client.saveDataFile( actualFile );
      }
      catch ( IOException exception )
      {
        LOG.log( Level.WARNING, "Saving capture data failed!", exception );
        JErrorDialog.showDialog( client, "Saving the capture data failed!", exception );
      }
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

/* EOF */
