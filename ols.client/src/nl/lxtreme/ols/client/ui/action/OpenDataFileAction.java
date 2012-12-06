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
package nl.lxtreme.ols.client.ui.action;


import static nl.lxtreme.ols.client.ui.action.FileExtensionUtils.*;
import static nl.lxtreme.ols.client.ui.icons.IconLocator.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import javax.swing.*;

import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.icons.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.log.*;


/**
 * Provides an "open data file" action.
 */
public class OpenDataFileAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "OpenDataFile";

  // CONSTRUCTORS

  /**
   * Creates a new {@link OpenDataFileAction} instance.
   */
  public OpenDataFileAction()
  {
    putValue( NAME, "Open ..." );
    putValue( SHORT_DESCRIPTION, "Open an existing data file" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_OPEN_DATAFILE ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_O ) );
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

    Window owner = SwingComponentUtils.getOwningWindow( aEvent );
    File file = SwingComponentUtils.showFileOpenDialog( owner, OLS_FILEFILTER );
    if ( file != null )
    {
      try
      {
        controller.openDataFile( file );

        client.setStatus( "Data loaded from {0} ...", file.getName() );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !IOUtil.handleInterruptedException( exception ) )
        {
          log.log( LogService.LOG_WARNING, "Loading OLS data file failed!", exception );

          JErrorDialog.showDialog( owner, "Loading OLS data file failed!", exception );

          client.setStatus( "Data file ({0}) loading failed ...", file.getName() );
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
