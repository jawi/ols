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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.action;


import static java.lang.Character.*;
import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.event.*;
import java.io.*;

import javax.swing.filechooser.*;

import org.slf4j.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a generic open action.
 */
public class OpenAction extends AbstractManagedAction
{
  // INNER TYPES

  static enum FileType
  {
    DATA_FILE, PROJECT_FILE, UNKNOWN;
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final Logger LOG = LoggerFactory.getLogger( OpenAction.class );

  public static final String OLS_FILE_EXTENSION = "ols";
  public static final String OLS_PROJECT_EXTENSION = "olp";

  public static final String ID = "Open";

  // CONSTRUCTORS

  /**
   * Creates a new {@link OpenAction} instance.
   */
  public OpenAction()
  {
    super( ID );

    putValue( NAME, "Open ..." );
    putValue( SHORT_DESCRIPTION, "Open an existing project or data file" );
    putValue( LARGE_ICON_KEY, IconFactory.createIcon( IconLocator.ICON_OPEN_PROJECT ) );

    putValue( ACCELERATOR_KEY, createMenuKeyMask( KeyEvent.VK_O ) );
    putValue( MNEMONIC_KEY, Integer.valueOf( KeyEvent.VK_P ) );

    putValue( MENU_NAME, ClientConstants.FILE_MENU );
    putValue( MENU_SEPARATOR_BELOW, Boolean.TRUE );
    putValue( MENU_ORDER, 1 );

    putValue( TOOLBAR_GROUP, ClientConstants.FILE_GROUP );
    putValue( TOOLBAR_ORDER, 0 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    Client client = getClient( aEvent );

    // Issue #62: in case the user does NOT confirm to lose its changes, we
    // should bail out immediately, otherwise continue normally...
    if ( client.isChanged() && //
        !askConfirmation( client, "You have unsaved changes.\nDo you really want to lose your changes?" ) )
    {
      return;
    }

    File file = showFileOpenDialog( client, new FileNameExtensionFilter( "OLS client files", OLS_FILE_EXTENSION, OLS_PROJECT_EXTENSION ) );
    if ( file == null )
    {
      // Cancelled by user...
      return;
    }

    try
    {
      FileType type = detectFileType( file );
      switch ( type )
      {
        case DATA_FILE:
          LOG.info( "Opening data file: {}", file );
          client.openDataFile( file );
          break;

        case PROJECT_FILE:
          LOG.info( "Opening project file: {}", file );
          client.openProjectFile( file );
          break;

        default:
          LOG.warn( "Unknown file: {}", file );
          JErrorDialog.showDialog( client, "Open failed", "Unable to open file " + file.getName(),
              "Unable to detect file type." );
          break;
      }
    }
    catch ( IOException exception )
    {
      LOG.warn( "Open file failed!", exception );
      JErrorDialog.showDialog( client, "Opening file '" + file.getName() + "' failed!", exception );
    }
  }

  /**
   * Tries to detect the file contents based on the first couple of bytes of the
   * file.
   * 
   * @param aFile
   *          the file to perform an auto-detect on;
   * @return the detected file type, or {@link FileType#UNKNOWN} in case auto
   *         detection failed.
   * @throws IOException
   *           in case of I/O problems.
   */
  private FileType detectFileType( File aFile )
  {
    FileType result = FileType.UNKNOWN;
    FileInputStream fis = null;
    byte[] buf = new byte[4];
    int read;

    try
    {
      fis = new FileInputStream( aFile );
      read = fis.read( buf );
    }
    catch ( IOException exception )
    {
      LOG.warn( "File detection failed!", exception );
      return result;
    }
    finally
    {
      if ( fis != null )
      {
        try
        {
          fis.close();
        }
        catch ( IOException exception )
        {
          // Ignore...
        }
      }
    }

    if ( read == buf.length )
    {
      if ( buf[0] == ';' && isLetter( buf[1] ) && isLetter( buf[2] ) && isLetter( buf[3] ) )
      {
        result = FileType.DATA_FILE;
      }
      else if ( buf[0] == 0x50 && buf[1] == 0x4b && buf[2] == 0x03 && buf[3] == 0x04 )
      {
        // 50 4B 03 04 indicates a ZIP file...
        result = FileType.PROJECT_FILE;
      }
    }

    return result;
  }
}
