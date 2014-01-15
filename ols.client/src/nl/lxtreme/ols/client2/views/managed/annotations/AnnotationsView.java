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
package nl.lxtreme.ols.client2.views.managed.annotations;


import static nl.lxtreme.ols.client2.ClientConstants.*;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.event.*;
import java.io.*;
import java.util.concurrent.*;

import javax.swing.*;
import javax.swing.filechooser.*;

import nl.lxtreme.ols.client2.icons.*;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.client2.views.managed.annotations.export.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.event.Event;

import com.jidesoft.docking.*;


/**
 * Provides a managed view that shows the defined cursors.
 */
public class AnnotationsView extends AbstractManagedView
{
  // CONSTANTS

  public static final String ID = "Annotations";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final ResettableTimer timer;

  private AnnotationTableModel tableModel;
  private JButton exportButton;
  private JLxTable table;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationsView} instance.
   */
  public AnnotationsView()
  {
    super( ID );

    this.timer = new ResettableTimer( new Runnable()
    {
      @Override
      public void run()
      {
        updateTableStructure();
      }
    }, 250, TimeUnit.MILLISECONDS );
  }

  // METHODS

  final void exportData( ActionEvent aEvent )
  {
    Window owner = SwingComponentUtils.getOwningWindow( aEvent );

    String[] extensions = ExportUtils.getExportExtensions();
    assert extensions != null;

    String preferredExtension = ( extensions.length == 0 ) ? "" : extensions[0];

    File exportFileName = SwingComponentUtils.showFileSaveDialog( owner, //
        new FileNameExtensionFilter( "Valid export format(s)", extensions ) );

    if ( exportFileName == null )
    {
      return;
    }

    File actualFile = exportFileName;
    String[] parts = SwingComponentUtils.stripFileExtension( exportFileName, extensions );
    String extension = parts[1];
    if ( extension == null )
    {
      extension = preferredExtension;
      actualFile = SwingComponentUtils.setFileExtension( exportFileName, extension );
    }

    FileOutputStream fos = null;
    BufferedWriter writer = null;

    try
    {
      DataFlavor dataFlavor = getDataFlavorByFileExtension( extension );
      TransferHandler transferHandler = this.table.getTransferHandler();

      fos = new FileOutputStream( actualFile );
      writer = new BufferedWriter( new OutputStreamWriter( fos ) );

      // Do not clobber the user's clipboard contents by this action...
      FakeClipboard clipboard = new FakeClipboard();
      transferHandler.exportToClipboard( this.table, clipboard, TransferHandler.COPY );

      writer.write( ( String )clipboard.getData( dataFlavor ) );
    }
    catch ( Exception exception )
    {
      JErrorDialog.showDialog( owner, "Export failed!", exception );
    }
    finally
    {
      try
      {
        writer.close();
      }
      catch ( IOException ignored )
      {
        // Ignored...
      }
      try
      {
        fos.close();
      }
      catch ( IOException ignored )
      {
        // Ignored...
      }
    }
  }

  final void jumpToAnnotation( long aTimestamp )
  {
    // TODO
  }

  /**
   * Updates the export button state.
   */
  final void updateButtonState()
  {
    final int rowCount = this.table.getModel().getRowCount();
    this.exportButton.setEnabled( rowCount > 0 );
  }

  final void updateTableStructure()
  {
    this.tableModel.updateStructure();

    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        updateButtonState();
      }
    } );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void build()
  {
    add( new JScrollPane( this.table ), BorderLayout.CENTER );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Annotations" );

    this.exportButton = new JButton( IconFactory.createIcon( IconLocator.ICON_SMALL_EXPORT ) );
    this.exportButton.setToolTipText( "Export annotation to file" );
    this.exportButton.setEnabled( false );
    this.exportButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( ActionEvent aEvent )
      {
        exportData( aEvent );
      }
    } );

    this.tableModel = new AnnotationTableModel();

    this.table = new AnnotationsTable();
    this.table.addMouseListener( new MouseAdapter()
    {
      @Override
      public void mouseClicked( final MouseEvent aEvent )
      {
        if ( aEvent.getClickCount() == 2 )
        {
          // double clicked...
          JTable source = ( JTable )aEvent.getSource();
          int rowIdx = source.getSelectedRow();
          if ( rowIdx >= 0 )
          {
            AnnotationTableModel model = ( AnnotationTableModel )source.getModel();

            DataAnnotation annotation = model.getAnnotation( rowIdx );
            if ( annotation != null )
            {
              long start = annotation.getStartTimestamp();
              long end = annotation.getEndTimestamp();

              // determine the middle of the annotation...
              long ptr = start + ( ( end - start ) / 2 );

              jumpToAnnotation( ptr );
            }
          }
        }
      }
    } );

    aFrame.setInitIndex( 0 );
    aFrame.setDockedHeight( 200 );

    JToolBar toolBar = new JToolBar();
    toolBar.add( this.exportButton );

    aFrame.setTitleBarComponent( toolBar );

    aContext.setInitSide( DockContext.DOCK_SIDE_SOUTH );
    aContext.setInitMode( DockContext.STATE_FRAMEDOCKED );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doUpdateState( ViewController aController, AcquisitionData aData )
  {
    if ( aController != null )
    {
      // Update the structure with the current session data...
      AnnotationData annotations = aController.getModel().getAnnotations();

      this.tableModel = new AnnotationTableModel( aData, annotations );
    }
    else
    {
      this.tableModel = new AnnotationTableModel();
    }

    this.table.setModel( this.tableModel );

    // Make the structure direct in order to get a direct visual clue...
    this.tableModel.updateStructure();

    updateButtonState();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String[] getEventTopics()
  {
    return new String[] { TOPIC_CLIENT_STATE.concat( "/*" ), TOPIC_ANNOTATIONS.concat( "/*" ) };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected boolean handleEvent( String aTopic, Event aEvent )
  {
    if ( aTopic.startsWith( TOPIC_ANNOTATIONS ) )
    {
      this.timer.schedule();

      return true;
    }

    return false;
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
}
