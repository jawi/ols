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
 * Copyright (C) 2006-2010 Michael Poppitz, www.sump.org
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.action;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.logging.*;

import javax.swing.filechooser.*;
import javax.swing.filechooser.FileFilter;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * 
 */
public class OpenProjectAction extends BaseAction
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
   * 
   */
  public OpenProjectAction( final ClientController aController )
  {
    super( ID, aController, ICON_OPEN_PROJECT, "Open project ...", "Open an existing project." );
    putValue( MNEMONIC_KEY, new Integer( KeyEvent.VK_P ) );
  }

  // METHODS

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    try
    {
      final File file = SwingComponentUtils.showFileOpenDialog( owner, OLS_PROJECT_FILTER );
      if ( ( file != null ) && file.isFile() )
      {
        if ( LOG.isLoggable( Level.INFO ) )
        {
          LOG.info( "Loading OLS project from file: " + file );
        }

        getController().openProjectFile( file );
      }
    }
    catch ( IOException exception )
    {
      // Make sure to handle IO-interrupted exceptions properly!
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        LOG.log( Level.WARNING, "Loading OLS project failed!", exception );
        JErrorDialog.showDialog( owner, "Loading OLS project failed!", exception );
      }
    }
  }
}

/* EOF */
