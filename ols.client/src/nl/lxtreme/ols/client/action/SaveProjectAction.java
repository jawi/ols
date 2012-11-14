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


import static nl.lxtreme.ols.client.icons.IconLocator.*;
import static nl.lxtreme.ols.client.action.FileExtensionUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.icons.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.log.*;


/**
 * Provides a "save" functionality for (existing) project.
 */
public class SaveProjectAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "SaveProject";

  // CONSTRUCTORS

  /**
   * Creates a new SaveProjectAction instance.
   * 
   * @param aController
   *          the controller to use in this action.
   */
  public SaveProjectAction()
  {
    putValue( NAME, "Save project" );
    putValue( SHORT_DESCRIPTION, "Save the current project" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_SAVE_PROJECT ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_S ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_A ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public final void actionPerformed( final ActionEvent aEvent )
  {
    Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    File file = getProjectFilename();
    if ( showFileChooserDialogNeeded() )
    {
      file = askForFilename( owner );
    }

    if ( file == null )
    {
      // User has canceled the file chooser dialog...
      return;
    }

    saveProjectFile( owner, file );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return ID;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState()
  {
    ProjectController controller = Client.getInstance().getProjectController();
    setEnabled( controller.isProjectChanged() );
  }

  /**
   * Returns whether or not a file chooser dialog is to be shown.
   * 
   * @return <code>true</code> if a file chooser dialog should be shown,
   *         <code>false</code> otherwise.
   */
  protected boolean showFileChooserDialogNeeded()
  {
    return getProjectFilename() == null;
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
  private File askForFilename( final Window owner )
  {
    final File file = SwingComponentUtils.showFileSaveDialog( owner, OLS_PROJECT_FILTER );
    if ( file != null )
    {
      return FileExtensionUtils.setFileExtension( file, OLS_PROJECT_EXTENSION );
    }
    return file;
  }

  /**
   * Returns the project's filename.
   * 
   * @return a file object denoting the project file to save the project to, can
   *         be <code>null</code> if no name is yet defined for the project.
   */
  private File getProjectFilename()
  {
    ProjectController controller = Client.getInstance().getProjectController();
    return controller.getProjectFile();
  }

  /**
   * Saves the project file.
   * 
   * @param aOwner
   *          the owning/parent window;
   * @param aFile
   *          the file to save the project to.
   */
  private void saveProjectFile( final Window aOwner, final File aFile )
  {
    final Client client = Client.getInstance();

    ProjectController controller = client.getProjectController();
    LogService log = client.getLogService();

    // Strip any "known" file extensions from the given value...
    final String projectName = FileExtensionUtils.stripFileExtension( aFile, OLS_FILE_EXTENSION, OLS_PROJECT_EXTENSION );

    try
    {
      controller.saveProjectFile( projectName, aFile );

      client.setStatus( "Project file ({0}) saved successfully ...", aFile.getName() );
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !IOUtil.handleInterruptedException( exception ) )
      {
        log.log( LogService.LOG_WARNING, "Saving OLS project file failed!", exception );

        JErrorDialog.showDialog( aOwner, "Saving OLS project file failed!", exception );

        client.setStatus( "Project file ({0}) saving failed ...", aFile.getName() );
      }
    }
  }
}

/* EOF */
