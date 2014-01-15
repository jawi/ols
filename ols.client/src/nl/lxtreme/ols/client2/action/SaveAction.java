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


import static nl.lxtreme.ols.client2.action.OpenAction.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.event.*;
import java.io.*;
import java.util.logging.*;

import javax.swing.filechooser.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a generic "save" action.
 */
public class SaveAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( SaveAction.class.getName() );

  public static final String ID = "Save";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SaveAction} instance.
   */
  public SaveAction()
  {
    this( ID, IconLocator.ICON_SAVE_DATAFILE, "Save ...", "Save acquired data to project or data file" );

    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_S ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 2 );

    putValue( TOOLBAR_GROUP, ClientConstants.FILE_GROUP );
    putValue( TOOLBAR_ORDER, 1 );
  }

  /**
   * Creates a new {@link SaveAction} instance.
   */
  protected SaveAction( String aId, String aIconName, String aName, String aDescription )
  {
    super( aId );

    putValue( NAME, aName );
    putValue( SHORT_DESCRIPTION, aDescription );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( aIconName ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    File file = client.getFile();
    if ( showFileChooserDialogNeeded( file ) )
    {
      file = showFileSaveDialog( client, new FileNameExtensionFilter( "OLS client files", OLS_FILE_EXTENSION, OLS_PROJECT_EXTENSION ) );
    }

    if ( file == null )
    {
      // User cancelled the dialog...
      return;
    }

    String ext = getFileExtension( file );
    if ( OLS_PROJECT_EXTENSION.equals( ext ) )
    {
      saveProjectFile( client, file );
    }
    else
    {
      // Assume we want to save the data file only...
      file = setFileExtension( file, OLS_FILE_EXTENSION );

      saveDataFile( client, file );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() && aClient.isChanged() );
  }

  /**
   * Returns whether or not a file chooser dialog is to be shown.
   * 
   * @return <code>true</code> if a file chooser dialog should be shown,
   *         <code>false</code> otherwise.
   */
  protected boolean showFileChooserDialogNeeded( File aFile )
  {
    return aFile == null;
  }

  /**
   * Saves the current acquired data to a plain data file.
   */
  private void saveDataFile( Client aClient, File aFile )
  {
    LOG.log( Level.INFO, "Saving data file: {0}", aFile );

    try
    {
      aClient.saveDataFile( aFile );
    }
    catch ( IOException exception )
    {
      LOG.log( Level.WARNING, "Saving data file failed!", exception );
      JErrorDialog.showDialog( aClient, "Saving the data file failed!", exception );
    }
  }

  /**
   * Saves the current acquired data to a project file.
   */
  private void saveProjectFile( Client aClient, File aFile )
  {
    LOG.log( Level.INFO, "Saving project file: {0}", aFile );

    try
    {
      aClient.saveProjectFile( aFile );
    }
    catch ( IOException exception )
    {
      LOG.log( Level.WARNING, "Saving project file failed!", exception );
      JErrorDialog.showDialog( aClient, "Saving the project file failed!", exception );
    }
  }
}

/* EOF */
