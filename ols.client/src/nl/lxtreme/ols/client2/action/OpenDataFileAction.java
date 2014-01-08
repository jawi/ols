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
 * Provides an "open data file" action.
 */
public class OpenDataFileAction extends AbstractFileAction
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
   * Creates a new {@link OpenDataFileAction} instance.
   */
  public OpenDataFileAction()
  {
    super( ID );

    putValue( NAME, "Open ..." );
    putValue( SHORT_DESCRIPTION, "Open an existing data file" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_OPEN_DATAFILE ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_O ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_ORDER, 4 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    final File file = SwingComponentUtils.showFileOpenDialog( client, OLS_FILEFILTER );
    if ( file != null )
    {
      LOG.log( Level.INFO, "Loading capture data from file {0}", file );

      try
      {
        client.openDataFile( file );
      }
      catch ( IOException exception )
      {
        LOG.log( Level.WARNING, "Loading OLS file failed!", exception );
        JErrorDialog.showDialog( client, "Loading the capture data failed!", exception );
      }
    }
  }
}

/* EOF */
