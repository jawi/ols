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
 * Provides an "open project" action.
 */
public class OpenProjectAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "OpenProject";

  // CONSTRUCTORS

  /**
   * Creates a new {@link OpenProjectAction} instance.
   */
  public OpenProjectAction()
  {
    putValue( NAME, "Open project ..." );
    putValue( SHORT_DESCRIPTION, "Open an existing project" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_OPEN_PROJECT ) );
    putValue( ACCELERATOR_KEY, SwingComponentUtils.createMenuKeyMask( KeyEvent.VK_O ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_P ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Client client = Client.getInstance();

    ProjectController controller = client.getProjectController();
    LogService log = client.getLogService();

    Window parent = SwingComponentUtils.getOwningWindow( aEvent );

    // Issue #62: in case the user does NOT confirm to lose its changes, we
    // should bail out immediately, otherwise continue normally...
    if ( controller.isProjectChanged() && //
        !SwingComponentUtils.askConfirmation( parent,
            "Current project has been changed.\nDo you really want to lose your changes?" ) )
    {
      return;
    }

    File file = SwingComponentUtils.showFileOpenDialog( parent, OLS_PROJECT_FILTER );
    if ( file != null )
    {
      try
      {
        controller.openProjectFile( file );

        client.setStatus( "Project file ({0}) loaded successfully ...", file.getName() );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !IOUtil.handleInterruptedException( exception ) )
        {
          log.log( LogService.LOG_WARNING, "Loading OLS project file failed!", exception );

          JErrorDialog.showDialog( parent, "Loading OLS project file failed!", exception );

          client.setStatus( "Project file ({0}) loading failed ...", file.getName() );
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
    return ID;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState()
  {
    // Always enabled...
    setEnabled( true );
  }
}

/* EOF */
