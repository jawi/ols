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

import javax.swing.filechooser.*;
import javax.swing.filechooser.FileFilter;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides an "open project" action.
 */
public class OpenProjectAction extends AbstractFileAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = Logger.getLogger( OpenProjectAction.class.getName() );

  public static final String OLS_PROJECT_EXTENSION = "olp";
  public static final FileFilter OLS_PROJECT_FILTER = new FileNameExtensionFilter( "OpenLogic Sniffer project file",
      OLS_PROJECT_EXTENSION );

  public static final String ID = "OpenProject";

  // CONSTRUCTORS

  /**
   * Creates a new {@link OpenProjectAction} instance.
   */
  public OpenProjectAction()
  {
    super( ID );

    putValue( NAME, "Open project ..." );
    putValue( SHORT_DESCRIPTION, "Open an existing project" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_OPEN_PROJECT ) );

    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_O ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_P ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 1 );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    // Issue #62: in case the user does NOT confirm to lose its changes, we
    // should bail out immediately, otherwise continue normally...
    if ( client.isProjectChanged() && //
        !SwingComponentUtils.askConfirmation( client,
            "Current project has been changed.\nDo you really want to lose your changes?" ) )
    {
      return;
    }

    final File file = SwingComponentUtils.showFileOpenDialog( client, OLS_PROJECT_FILTER );
    if ( file != null )
    {
      LOG.log( Level.INFO, "Loading project data from file: {0}", file );

      try
      {
        client.openProjectFile( file );
      }
      catch ( IOException exception )
      {
        LOG.log( Level.WARNING, "Loading OLS project failed!", exception );
        JErrorDialog.showDialog( client, "Loading the project data failed!", exception );
      }
    }
  }
}

/* EOF */
