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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.signaldisplay;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a table-like view of {@link AcquisitionData}, in which data is shown
 * as numeric values in rows and columns.
 */
public class StateViewComponent extends JPanel
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SignalDiagramController controller;

  private JLxTable table;

  // CONSTRUCTORS

  /**
   * Creates a new {@link StateViewComponent} instance.
   *
   * @param aController
   *          the controller to use, cannot be <code>null</code>.
   */
  public StateViewComponent( final SignalDiagramController aController )
  {
    super( new BorderLayout() );

    this.controller = aController;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    final StateTableModel tableModel = new StateTableModel( this.controller.getViewModel() );

    this.table = new JLxTable( tableModel );
    this.table.setShowVerticalLines( false );
    this.table.setShowHorizontalLines( true );
    this.table.setAutoCreateRowSorter( true );
    this.table.setAutoCreateColumnsFromModel( false );
    this.table.setDefaultRenderer( Integer.class, new DataCellRenderer() );
    this.table.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );

    this.table.getTableHeader().addMouseListener( new MouseAdapter()
    {
      @Override
      public void mousePressed( final MouseEvent aEvent )
      {
        handlePopup( aEvent );
      }

      @Override
      public void mouseReleased( final MouseEvent aEvent )
      {
        handlePopup( aEvent );
      }

      private void handlePopup( final MouseEvent aEvent )
      {
        final int columnIdx = StateViewComponent.this.table.columnAtPoint( aEvent.getPoint() );
        if ( aEvent.isPopupTrigger() && ( columnIdx > 0 ) )
        {
          final Radix mode = tableModel.getRadix( columnIdx );
          final ButtonGroup group = new ButtonGroup();
          final JPopupMenu menu = new JPopupMenu();

          for ( final Radix vm : Radix.values() )
          {
            JMenuItem item = new JRadioButtonMenuItem( vm.getDisplayName() );
            item.setSelected( vm == mode );
            item.addActionListener( new ActionListener()
            {
              @Override
              public void actionPerformed( final ActionEvent aE )
              {
                tableModel.setViewMode( columnIdx, vm );
                tableModel.fireColumnDataChanged( columnIdx );
                // Ensure the column header is updated as well...
                StateViewComponent.this.table.resetColumnHeader( columnIdx );
              }
            } );

            menu.add( item );
            group.add( item );
          }

          menu.show( aEvent.getComponent(), aEvent.getX(), aEvent.getY() );
        }
      }
    } );

    add( new JScrollPane( this.table ), BorderLayout.CENTER );

    super.addNotify();
  }

  /**
   * {@inheritDoc}
   */
  public double getDisplayedInterval()
  {
    Rectangle visibleRect = this.table.getVisibleRect();

    int startRow = this.table.rowAtPoint( new Point( 0, visibleRect.y ) );
    int endRow = this.table.rowAtPoint( new Point( 0, visibleRect.y + visibleRect.height ) );

    return endRow - startRow;
  }

  /**
   * @param aTimestamp
   */
  public void scrollToTimestamp( final long aTimestamp )
  {
    // TODO Auto-generated method stub

  }
}
