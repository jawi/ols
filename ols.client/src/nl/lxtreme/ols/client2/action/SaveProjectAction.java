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


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.logging.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a "save" functionality for (existing) project.
 */
public class SaveProjectAction extends AbstractManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( SaveProjectAction.class.getName() );

  public static final String ID = "SaveProject";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SaveProjectAction} instance.
   */
  public SaveProjectAction()
  {
    this( ID, IconLocator.ICON_SAVE_PROJECT, "Save project", "Save the current project" );
  }

  /**
   * Creates a new SaveProjectAction instance.
   * 
   * @param aID
   *          the ID of this action;
   * @param aController
   *          the controller to use;
   * @param aIconName
   *          the (optional) name of the icon;
   * @param aName
   *          the name of this action;
   * @param aDescription
   *          the description/tooltip of this action.
   */
  protected SaveProjectAction( String aID, String aIconName, String aName, String aDescription )
  {
    super( aID );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_S ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_A ) );

    putValue( NAME, aName );
    putValue( SHORT_DESCRIPTION, aDescription );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( aIconName ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 2 );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    File file = client.getProjectFile();
    if ( showFileChooserDialogNeeded( file ) )
    {
      file = askForFilename( client );
    }

    if ( file == null )
    {
      // User has canceled the file chooser dialog...
      return;
    }

    saveProjectFile( client, file );
  }

  /**
   * Asks the user to specify a filename.
   * 
   * @param owner
   *          the parent/owner window of the file chooser dialog, can be
   *          <code>null</code>.
   * @return the file specified by the user, or <code>null</code> if the user
   *         has cancelled the action.
   */
  protected File askForFilename( final Window owner )
  {
    final File file = SwingComponentUtils.showFileSaveDialog( owner, OpenProjectAction.OLS_PROJECT_FILTER );
    if ( file != null )
    {
      return FileUtils.setFileExtension( file, OpenProjectAction.OLS_PROJECT_EXTENSION );
    }
    return file;
  }

  /**
   * Saves the project file.
   * 
   * @param aOwner
   *          the owning/parent window;
   * @param aFile
   *          the file to save the project to.
   */
  protected void saveProjectFile( Client aClient, File aFile )
  {
    LOG.log( Level.INFO, "Saving project data to file: {0}", aFile );

    // Strip any "known" file extensions from the given value...
    final String projectName = FileUtils.stripFileExtension( aFile, OpenDataFileAction.OLS_FILE_EXTENSION,
        OpenProjectAction.OLS_PROJECT_EXTENSION )[0];

    try
    {
      aClient.saveProjectFile( projectName, aFile );
    }
    catch ( IOException exception )
    {
      LOG.log( Level.WARNING, "Saving OLS project failed!", exception );
      JErrorDialog.showDialog( aClient, "Saving the project data failed!", exception );
    }
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
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    setEnabled( aClient.hasAcquiredData() );
  }
}

/* EOF */
