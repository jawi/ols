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
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;
import java.awt.datatransfer.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.table.*;


/**
 * Provides a "better" JTable in the sense that this table-implementation allows
 * the copy-action to use the rendered values instead of the raw table values.
 */
public class JLxTable extends JTable
{
  // INNER TYPES

  /**
   * Provides an adapter for table cell renders, allowing cell customizations.
   */
  public static interface TableCellRendererAdapter
  {
    // METHODS

    /**
     * @param aCellComponent
     * @param aCellValue
     * @return the adapted cell render component, never <code>null</code>.
     */
    Component render( Component aCellComponent, Object aCellValue, boolean aIsSelected );
  }

  /**
   * Provides an implementation of {@link Transferable} for handling table data
   * in various data flavors.
   */
  protected static class TableDataTransferable implements Transferable
  {
    // VARIABLES

    private final JLxTable table;

    // CONSTRUCTORS

    /**
     * Creates a new {@link TableDataTransferable} instance.
     * 
     * @param aTable
     *          the table to transfer the data of, cannot be <code>null</code>.
     */
    public TableDataTransferable( final JLxTable aTable )
    {
      this.table = aTable;
    }

    // METHODS

    /**
     * Returns an object which represents the data to be transferred. The class
     * of the object returned is defined by the representation class of the
     * flavor.
     * 
     * @param aFlavor
     *          the requested flavor for the data
     * @see DataFlavor#getRepresentationClass
     * @exception IOException
     *              if the data is no longer available in the requested flavor.
     * @exception UnsupportedFlavorException
     *              if the requested data flavor is not supported.
     */
    @Override
    public Object getTransferData( final DataFlavor aFlavor ) throws UnsupportedFlavorException, IOException
    {
      Object data = this.table.getTableData( aFlavor );

      String text = ( data == null ) ? "" : String.valueOf( data );

      final Class<?> representationClass = aFlavor.getRepresentationClass();
      if ( String.class.equals( representationClass ) )
      {
        return text;
      }
      else if ( Reader.class.equals( representationClass ) )
      {
        return new StringReader( text );
      }
      else if ( InputStream.class.equals( representationClass ) )
      {
        return new ByteArrayInputStream( text.getBytes( "utf-8" ) );
      }

      throw new UnsupportedFlavorException( aFlavor );
    }

    /**
     * Returns an array of DataFlavor objects indicating the flavors the data
     * can be provided in. The array should be ordered according to preference
     * for providing the data (from most richly descriptive to least
     * descriptive).
     * 
     * @return an array of data flavors in which this data can be transferred
     */
    @Override
    public DataFlavor[] getTransferDataFlavors()
    {
      DataFlavor[] dataFlavors = this.table.getTableDataFlavors();
      if ( dataFlavors == null )
      {
        return new DataFlavor[0];
      }
      return Arrays.copyOf( dataFlavors, dataFlavors.length );
    }

    /**
     * Returns whether or not the specified data flavor is supported for this
     * object.
     * 
     * @param aFlavor
     *          the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    @Override
    public boolean isDataFlavorSupported( final DataFlavor aFlavor )
    {
      DataFlavor[] flavors = getTransferDataFlavors();
      return findFlavor( flavors, aFlavor );
    }

    /**
     * @param aFlavors
     * @param aFlavor
     * @return
     */
    private boolean findFlavor( final DataFlavor[] aFlavors, final DataFlavor aFlavor )
    {
      for ( DataFlavor flavor : aFlavors )
      {
        if ( flavor.equals( aFlavor ) )
        {
          return true;
        }
      }
      return false;
    }
  }

  /**
   * Provides a custom table transfer handler using the table cell renderers
   * instead of the raw table values.
   */
  protected static class TableTransferHandler extends TransferHandler implements UIResource
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    @Override
    public int getSourceActions( final JComponent aComponent )
    {
      return COPY;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Transferable createTransferable( final JComponent aComponent )
    {
      if ( !( aComponent instanceof JLxTable ) )
      {
        return null;
      }

      JLxTable table = ( JLxTable )aComponent;
      if ( !table.getRowSelectionAllowed() && !table.getColumnSelectionAllowed() )
      {
        return null;
      }

      return new TableDataTransferable( table );
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  public static final String DATA_FLAVOR_HTML = "text/html";
  public static final String DATA_FLAVOR_CSV = "text/csv";
  public static final String DATA_FLAVOR_TEXT = "text/plain";

  private static final Color NORMAL_ROW_COLOR = Color.WHITE;
  private static final Color ALT_ROW_COLOR = new Color( 0xf2, 0xf2, 0xf4 );

  // VARIABLES

  private TableCellRendererAdapter cellRendererAdapter;

  // CONSTRUCTORS

  /**
   * Creates a new {@link JLxTable} instance.
   */
  public JLxTable()
  {
    super();

    setTransferHandler( new TableTransferHandler() );
  }

  /**
   * Creates a new {@link JLxTable} instance.
   */
  public JLxTable( final TableModel aTableModel )
  {
    super( aTableModel );

    setTransferHandler( new TableTransferHandler() );
  }

  /**
   * Creates a new {@link JLxTable} instance.
   */
  public JLxTable( final TableModel aTableModel, final TableColumnModel aColumnModel )
  {
    super( aTableModel, aColumnModel );

    setTransferHandler( new TableTransferHandler() );
  }

  // METHODS

  /**
   * Returns the cell renderer adapter.
   * 
   * @return the cell renderer adapter, never <code>null</code>.
   */
  public TableCellRendererAdapter getCellRendererAdapter()
  {
    return this.cellRendererAdapter;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component prepareRenderer( final TableCellRenderer aRenderer, final int aRow, final int aColumn )
  {
    boolean isSelected = false;
    boolean hasFocus = false;

    // Only indicate the selection and focused cell if not printing
    if ( !isPaintingForPrint() )
    {
      isSelected = isCellSelected( aRow, aColumn );

      boolean rowIsLead = ( this.selectionModel.getLeadSelectionIndex() == aRow );
      boolean colIsLead = ( this.columnModel.getSelectionModel().getLeadSelectionIndex() == aColumn );

      hasFocus = ( rowIsLead && colIsLead ) && isFocusOwner();
    }

    final Object value = getValueAt( aRow, aColumn );

    Component comp = aRenderer.getTableCellRendererComponent( this, value, isSelected, hasFocus, aRow, aColumn );
    if ( !isSelected )
    {
      comp.setBackground( ( ( aRow % 2 ) != 0 ) ? getAlternativeCellBackgroundColor() : getNormalCellBackgroundColor() );
    }

    TableCellRendererAdapter adapter = getCellRendererAdapter();
    if ( adapter != null )
    {
      comp = adapter.render( comp, value, isSelected );
    }

    // Disable the component in case this table is not enabled...
    comp.setEnabled( isEnabled() );

    return comp;
  }

  /**
   * Resets the column header to the value from the contained {@link TableModel}
   * .
   * 
   * @param aColumnIndex
   *          the index of the column to reset the header value for.
   */
  public void resetColumnHeader( int aColumnIndex )
  {
    setColumnHeader( aColumnIndex, getModel().getColumnName( aColumnIndex ) );
  }

  /**
   * Sets the cell renderer adapter.
   * 
   * @param aCellRendererAdapter
   *          the cell renderer adapter to set, cannot be <code>null</code>.
   */
  public void setCellRendererAdapter( final TableCellRendererAdapter aCellRendererAdapter )
  {
    if ( aCellRendererAdapter == null )
    {
      throw new IllegalArgumentException( "CellRendererAdapter cannot be null!" );
    }
    this.cellRendererAdapter = aCellRendererAdapter;
  }

  /**
   * Sets the column header to a given value.
   * 
   * @param aColumnIndex
   *          the index of the column to reset the header value for;
   * @param aValue
   *          the new value of the column header to set, can be
   *          <code>null</code>.
   */
  public void setColumnHeader( int aColumnIndex, Object aValue )
  {
    TableColumn column = getColumnModel().getColumn( aColumnIndex );
    column.setHeaderValue( aValue );
    getTableHeader().repaint( 50L );
  }

  /**
   * Overridden in order to preserve the column widths.
   * 
   * @see JTable#setModel(TableModel)
   */
  @Override
  public void setModel( TableModel aDataModel )
  {
    int[] widths = getColumnWidths();
    super.setModel( aDataModel );
    setColumnWidths( widths );
  }

  /**
   * Returns the "alternative" color for cell backgrounds.
   * 
   * @return a color, never <code>null</code>.
   */
  protected Color getAlternativeCellBackgroundColor()
  {
    Color color = UIManager.getColor( "jlxtable.alternativeCellBackground" );
    if ( color == null )
    {
      color = ALT_ROW_COLOR;
    }
    return color;
  }

  /**
   * Returns the "normal" color for cell backgrounds.
   * 
   * @return a color, never <code>null</code>.
   */
  protected Color getNormalCellBackgroundColor()
  {
    Color color = UIManager.getColor( "jlxtable.normalCellBackground" );
    if ( color == null )
    {
      color = NORMAL_ROW_COLOR;
    }
    return color;
  }

  /**
   * @param aFlavor
   *          the data flavor to get the table data for, cannot be
   *          <code>null</code>.
   * @return an object representing according to the requested formatting, or
   *         <code>null</code> if no data is available.
   * @throws UnsupportedFlavorException
   *           in case the given flavor is not supported.
   * @throws IOException
   *           in case of I/O problems getting or formatting the table data.
   */
  protected Object getTableData( final DataFlavor aFlavor ) throws UnsupportedFlavorException, IOException
  {
    return null;
  }

  /**
   * @return the data flavors supported by this table.
   */
  protected DataFlavor[] getTableDataFlavors()
  {
    return null;
  }

  /**
   * @return the current widths of the columns, as array, never
   *         <code>null</code>.
   */
  private int[] getColumnWidths()
  {
    TableColumnModel columnModel = getColumnModel();
    int[] widths = new int[columnModel.getColumnCount()];
    for ( int i = 0; i < widths.length; i++ )
    {
      widths[i] = columnModel.getColumn( i ).getWidth();
    }
    return widths;
  }

  /**
   * @param aWidths
   *          the array with column widths to apply.
   */
  private void setColumnWidths( int... aWidths )
  {
    TableColumnModel columnModel = getColumnModel();
    int len = Math.min( columnModel.getColumnCount(), aWidths.length );
    for ( int i = 0; i < len; i++ )
    {
      columnModel.getColumn( i ).setWidth( aWidths[i] );
    }
  }
}
