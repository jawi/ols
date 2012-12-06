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
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.log.*;


/**
 * Provides a "save data file" action.
 */
public class SaveDataFileAction extends AbstractAction implements IManagedAction
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String ID = "SaveDataFile";

  // CONSTRUCTORS

  /**
   * Creates a new {@link SaveDataFileAction} instance.
   */
  public SaveDataFileAction()
  {
    putValue( NAME, "Save ..." );
    putValue( SHORT_DESCRIPTION, "Save data file" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( ICON_SAVE_DATAFILE ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_S ) );
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

    File file = SwingComponentUtils.showFileSaveDialog( owner, OLS_FILEFILTER );
    if ( file != null )
    {
      final File actualFile = FileExtensionUtils.setFileExtension( file, OLS_FILE_EXTENSION );

      try
      {
        controller.saveDataFile( actualFile );

        client.setStatus( "Data saved to {0} ...", file.getName() );
      }
      catch ( IOException exception )
      {
        // Make sure to handle IO-interrupted exceptions properly!
        if ( !IOUtil.handleInterruptedException( exception ) )
        {
          log.log( LogService.LOG_WARNING, "Saving OLS data file failed!", exception );

          JErrorDialog.showDialog( owner, "Saving OLS data file failed!", exception );

          client.setStatus( "Data file ({0}) saving failed ...", file.getName() );
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
    setEnabled( hasCapturedData() );
  }

  /**
   * @return <code>true</code> if there is data captured to export,
   *         <code>false</code> otherwise.
   */
  private boolean hasCapturedData()
  {
    final Session session = Client.getInstance().getSession();
    return session.hasData();
  }
}

/* EOF */
