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
package nl.lxtreme.ols.client2.views.managed.cursors;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.client2.views.managed.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.acquisition.Cursor;
import nl.lxtreme.ols.util.swing.component.*;

import com.jidesoft.docking.*;


/**
 * Provides a managed view that shows the defined cursors.
 */
public class CursorDetailsView extends AbstractManagedView
{
  // CONSTANTS

  public static final String ID = "CursorDetails";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  // Injected by Felix DM...
  private volatile ActionManager actionManager;

  private JLxTable table;
  private MouseListener listener;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CursorDetailsView} instance.
   */
  public CursorDetailsView()
  {
    super( ID );
  }

  // METHODS

  /**
   * Jumps to the cursor denoted by a given row index in the current table.
   * 
   * @param aController
   * @param aRowIndex
   */
  final void jumpToCursor( ViewController aController, int aRowIndex )
  {
    CursorTableModel model = ( CursorTableModel )this.table.getModel();
    Cursor cursor = model.getCursor( aRowIndex );

    aController.scrollToTimestamp( cursor.getTimestamp() );
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
    setName( "Cursor details" );

    this.table = new JLxTable( new CursorTableModel() );
    this.table.setAutoResizeMode( JTable.AUTO_RESIZE_LAST_COLUMN );
    this.table.setAutoCreateColumnsFromModel( false );
    this.table.setIntercellSpacing( new Dimension( 2, 2 ) );
    this.table.setColumnSelectionAllowed( false );
    this.table.setCellSelectionEnabled( false );
    this.table.setRowSelectionAllowed( true );
    this.table.setAutoCreateRowSorter( true );
    this.table.setFillsViewportHeight( true );
    this.table.setShowGrid( false );

    TableColumnModel columnModel = this.table.getColumnModel();
    columnModel.getColumn( 0 ).setPreferredWidth( 25 );
    columnModel.getColumn( 1 ).setPreferredWidth( 100 );

    JCheckBox cursorsVisible = new JCheckBox( this.actionManager.getAction( SetCursorsVisibleAction.ID ) );
    JCheckBox snapCursors = new JCheckBox( this.actionManager.getAction( SetCursorSnapModeAction.ID ) );

    JToolBar toolBar = new JToolBar( JToolBar.VERTICAL );
    toolBar.setFloatable( false );
    toolBar.add( cursorsVisible );
    toolBar.add( snapCursors );

    aFrame.setTitleBarComponent( toolBar );
    aFrame.setInitIndex( 0 );

    aContext.setInitSide( DockContext.DOCK_SIDE_EAST );
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
      Cursor[] cursors = aData.getCursors();

      updateViewText( aController, aData.areCursorsVisible(), cursors );
    }
    else
    {
      updateViewText( null, false );
    }
  }

  private void updateViewText( final ViewController aController, boolean aCursorsVisible, Cursor... aCursors )
  {
    CursorTableModel tableModel = new CursorTableModel( aCursors );
    // Update only when necessary...
    if ( !tableModel.equals( this.table.getModel() ) )
    {
      // Remove old listener (contains a reference to our old view
      // controller!)...
      if ( this.listener != null )
      {
        this.table.removeMouseListener( this.listener );
      }

      // Update the entire model...
      this.table.setModel( tableModel );

      // Create & install new listener using the given view controller...
      this.listener = new MouseAdapter()
      {
        @Override
        public void mouseClicked( final MouseEvent aEvent )
        {
          JTable source = ( JTable )aEvent.getSource();
          int rowIdx = source.getSelectedRow();
          if ( ( aEvent.getClickCount() == 2 ) && ( rowIdx >= 0 ) )
          {
            // double clicked on a row...
            jumpToCursor( aController, rowIdx );
          }
        }
      };
      this.table.addMouseListener( this.listener );

      // Add a custom selection model that also highlights the selected cursor
      // in the main view...
      this.table.setSelectionModel( new DefaultListSelectionModel()
      {
        private static final long serialVersionUID = 1L;

        @Override
        public void setSelectionInterval( int aIndex0, int aIndex1 )
        {
          super.setSelectionInterval( aIndex0, aIndex1 );

          CursorTableModel model = ( CursorTableModel )table.getModel();
          Cursor selectedCursor = model.getCursor( aIndex1 );

          aController.setSelectedCursor( selectedCursor );
        }
      } );
    }
    else
    {
      // Model itself is not changed, simply update the table contents...
      ( ( CursorTableModel )this.table.getModel() ).fireTableDataChanged();
    }

    this.table.setEnabled( aCursorsVisible );
  }
}
