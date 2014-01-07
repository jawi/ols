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
package nl.lxtreme.ols.client2.views.state;


import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * Provides a table-like view of {@link AcquisitionData}, in which data is shown
 * as numeric values in rows and columns.
 */
public class StateView extends BaseView
{
  // INNER TYPES

  static class DataCellRenderer extends DefaultTableCellRenderer
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    @Override
    public Component getTableCellRendererComponent( JTable aTable, Object aValue, boolean aIsSelected,
        boolean aHasFocus, int aRow, int aColumn )
    {
      JLabel label = ( JLabel )super.getTableCellRendererComponent( aTable, aValue, aIsSelected, aHasFocus, aRow,
          aColumn );

      StateTableModel tableModel = ( StateTableModel )aTable.getModel();

      int width = tableModel.getViewWidth( aColumn );
      int base = tableModel.getViewMode( aColumn ).base;

      label.setHorizontalAlignment( SwingConstants.RIGHT );
      label.setText( toString( ( Integer )aValue, width, base ) );
      return label;
    }

    private String toString( Integer aValue, int aWidth, int aBase )
    {
      StringBuilder text = new StringBuilder( aWidth );
      text.append( Integer.toString( aValue.intValue(), aBase ).toUpperCase( Locale.ENGLISH ) );
      while ( text.length() < aWidth )
      {
        text.insert( 0, '0' );
      }
      return text.toString();
    }
  }

  static class StateTableModel extends AbstractTableModel
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // VARIABLES

    private final int[] values;
    private final long[] timestamps;
    private final ChannelGroup[] groups;
    private final ViewMode[] viewModes;

    // CONSTRUCTORS

    /**
     * Creates a new {@link StateTableModel} instance.
     */
    public StateTableModel( AcquisitionData aData )
    {
      this.values = aData.getValues();
      this.timestamps = aData.getTimestamps();
      this.groups = aData.getChannelGroups();

      this.viewModes = new ViewMode[this.groups.length];
      Arrays.fill( viewModes, ViewMode.HEX );
    }

    // METHODS

    /**
     * Convenience method to fire an event when the data of an entire column is
     * changed.
     * 
     * @param aColumnIndex
     *          the index of the column that is changed.
     */
    public void fireColumnDataChanged( int aColumnIndex )
    {
      int first = 0;
      int last = getRowCount() - 1;

      fireTableChanged( new TableModelEvent( this, first, last, aColumnIndex ) );
    }

    @Override
    public Class<?> getColumnClass( int aColumnIndex )
    {
      if ( aColumnIndex == 0 )
      {
        return Long.class;
      }
      return Integer.class;
    }

    @Override
    public int getColumnCount()
    {
      return 1 + ( this.groups != null ? this.groups.length : 0 );
    }

    @Override
    public String getColumnName( int aColumn )
    {
      String name = super.getColumnName( aColumn );
      String sub = null;
      if ( aColumn == 0 )
      {
        name = "State";
        sub = "#";
      }
      else
      {
        int groupIdx = aColumn - 1;
        if ( this.groups != null && groupIdx < this.groups.length )
        {
          ChannelGroup group = this.groups[groupIdx];
          ViewMode vm = this.viewModes[groupIdx];

          name = group.getName();
          sub = Integer.toString( vm.base );
        }
      }

      StringBuilder sb = new StringBuilder();
      sb.append( "<html><body><b>" ).append( name ).append( "</b>" );
      if ( sub != null )
      {
        sb.append( "<sub>" ).append( sub ).append( "</sub>" );
      }
      sb.append( "</body></html>" );

      return sb.toString();
    }

    @Override
    public int getRowCount()
    {
      return this.values.length;
    }

    @Override
    public Object getValueAt( int aRowIndex, int aColumnIndex )
    {
      if ( aColumnIndex == 0 )
      {
        return this.timestamps[aRowIndex];
      }

      int groupIdx = aColumnIndex - 1;
      if ( this.groups == null || groupIdx >= this.groups.length )
      {
        return null;
      }

      int sample = this.values[aRowIndex];
      int result = 0;

      Channel[] channels = this.groups[groupIdx].getChannels();
      for ( int i = channels.length - 1; i >= 0; i-- )
      {
        result <<= 1;
        if ( ( sample & channels[i].getMask() ) != 0 )
        {
          result |= 1;
        }
      }

      return result;
    }

    public ViewMode getViewMode( int aColumnIndex )
    {
      if ( aColumnIndex == 0 )
      {
        return ViewMode.DEC;
      }
      else
      {
        return this.viewModes[aColumnIndex - 1];
      }
    }

    public int getViewWidth( int aColumnIndex )
    {
      if ( aColumnIndex == 0 )
      {
        return -1;
      }

      int groupIdx = aColumnIndex - 1;

      int channels = this.groups[groupIdx].getChannels().length;
      double width = this.viewModes[groupIdx].width;

      return ( int )Math.ceil( channels / width );
    }

    public void setViewMode( int aColumnIndex, ViewMode aMode )
    {
      if ( aColumnIndex > 0 )
      {
        this.viewModes[aColumnIndex - 1] = aMode;
      }
    }
  }

  static enum ViewMode
  {
    HEX( "Hexadecimal", 16 ), DEC( "Decimal", 10 ), OCT( "Octal", 8 ), BIN( "Binary", 2 );

    final String name;
    final int base;
    final double width;

    private ViewMode( String aName, int aBase )
    {
      this.name = aName;
      this.base = aBase;
      this.width = Math.log10( aBase ) / Math.log10( 2 );
    }

    @Override
    public String toString()
    {
      return this.name;
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private JLxTable table;

  // CONSTRUCTORS

  /**
   * Creates a new {@link StateView} instance.
   * 
   * @param aController
   *          the controller to use, cannot be <code>null</code>;
   * @param aModel
   *          the model to use, cannot be <code>null</code>.
   */
  public StateView( ViewController aController, ViewModel aModel )
  {
    super( aController, aModel );
  }

  @Override
  public void initialize()
  {
    final StateTableModel tableModel = new StateTableModel( this.model.getData() );

    this.table = new JLxTable( tableModel );
    this.table.setShowVerticalLines( false );
    this.table.setShowHorizontalLines( true );
    this.table.setAutoCreateRowSorter( true );
    this.table.setAutoCreateColumnsFromModel( false );
    this.table.setDefaultRenderer( Integer.class, new DataCellRenderer() );
    this.table.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );

    this.table.getTableHeader().addMouseListener( new MouseAdapter()
    {
      public void mousePressed( MouseEvent aEvent )
      {
        handlePopup( aEvent );
      }

      public void mouseReleased( MouseEvent aEvent )
      {
        handlePopup( aEvent );
      }

      private void handlePopup( MouseEvent aEvent )
      {
        final int columnIdx = table.columnAtPoint( aEvent.getPoint() );
        if ( aEvent.isPopupTrigger() && columnIdx > 0 )
        {
          final ViewMode mode = tableModel.getViewMode( columnIdx );
          final ButtonGroup group = new ButtonGroup();
          final JPopupMenu menu = new JPopupMenu();

          for ( final ViewMode vm : ViewMode.values() )
          {
            JMenuItem item = new JRadioButtonMenuItem( vm.name );
            item.setSelected( vm == mode );
            item.addActionListener( new ActionListener()
            {
              @Override
              public void actionPerformed( ActionEvent aE )
              {
                tableModel.setViewMode( columnIdx, vm );
                tableModel.fireColumnDataChanged( columnIdx );
                // Ensure the column header is updated as well...
                table.resetColumnHeader( columnIdx );
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
  }
}
