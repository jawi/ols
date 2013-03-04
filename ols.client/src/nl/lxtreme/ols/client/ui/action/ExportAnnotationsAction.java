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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui.action;


import static nl.lxtreme.ols.client.ui.action.FileExtensionUtils.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.event.*;
import java.io.*;

import javax.swing.*;
import javax.swing.filechooser.*;
import nl.lxtreme.ols.client.ui.*;
import nl.lxtreme.ols.client.ui.export.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides an action for exporting annotations to a HTML/CSV report.
 */
public class ExportAnnotationsAction extends AbstractAction implements IManagedAction
{
  // INNER TYPES

  public static interface ExportDataProvider
  {
    JLxTable getTable();
  }

  private static class FakeClipboard extends Clipboard
  {
    // CONSTRUCTORS

    /**
     * Creates a new {@link FakeClipboard} instance.
     */
    public FakeClipboard()
    {
      super( "FakeClipboard" );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final String ID = "ExportAnnotations";

  // VARIABLES

  private final ExportDataProvider dataProvider;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ExportAnnotationsAction} instance.
   */
  public ExportAnnotationsAction( final ExportDataProvider aDataProvider )
  {
    super( "Export ..." );

    this.dataProvider = aDataProvider;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void actionPerformed( final ActionEvent aEvent )
  {
    final Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    final String[] extensions = ExportUtils.getExportExtensions();
    assert extensions != null;

    final String preferredExtension = ( extensions.length == 0 ) ? "" : extensions[0];

    final File exportFileName = SwingComponentUtils.showFileSaveDialog( owner, //
        new FileNameExtensionFilter( "Valid export format(s)", extensions ) );

    if ( exportFileName != null )
    {
      final JTable table = this.dataProvider.getTable();

      File actualFile = exportFileName;
      String[] parts = stripFileExtension( exportFileName, extensions );
      String extension = parts[1];
      if ( extension == null )
      {
        extension = preferredExtension;
        actualFile = setFileExtension( exportFileName, extension );
      }

      FileOutputStream fos = null;
      BufferedWriter writer = null;

      try
      {
        DataFlavor dataFlavor = getDataFlavorByFileExtension( extension );
        TransferHandler transferHandler = table.getTransferHandler();

        fos = new FileOutputStream( actualFile );
        writer = new BufferedWriter( new OutputStreamWriter( fos ) );

        // Do not clobber the user's clipboard contents by this action...
        FakeClipboard clipboard = new FakeClipboard();
        transferHandler.exportToClipboard( table, clipboard, TransferHandler.COPY );

        writer.write( ( String )clipboard.getData( dataFlavor ) );
      }
      catch ( Exception exception )
      {
        JErrorDialog.showDialog( owner, "Export failed!", exception );
      }
      finally
      {
        IOUtil.closeResource( writer );
        IOUtil.closeResource( fos );
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
    setEnabled( hasAnnotationData() );
  }

  /**
   * @param aFile
   * @return
   * @throws ClassNotFoundException
   */
  private DataFlavor getDataFlavorByFileExtension( final String aExtension ) throws ClassNotFoundException
  {
    String flavor = JLxTable.DATA_FLAVOR_CSV;
    if ( "html".equals( aExtension ) || "htm".equals( aExtension ) )
    {
      flavor = JLxTable.DATA_FLAVOR_HTML;
    }
    else if ( "text".equals( aExtension ) || "txt".equals( aExtension ) )
    {
      flavor = JLxTable.DATA_FLAVOR_TEXT;
    }

    return new DataFlavor( flavor.concat( ";charset=unicode;class=java.lang.String" ) );
  }

  /**
   * @return <code>true</code> if there are annotations to export,
   *         <code>false</code> otherwise.
   */
  private boolean hasAnnotationData()
  {
    final Session session = Client.getInstance().getSession();
    // Session can only be null in cases where the client is starting up or
    // shutting down...
    return ( session != null ) && session.hasData();
  }
}
