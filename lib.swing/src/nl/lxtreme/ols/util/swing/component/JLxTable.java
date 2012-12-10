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
import java.util.List;

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
    // CONSTANTS

    private static final DataFlavor[] HTML_FLAVORS;
    private static final DataFlavor[] CSV_FLAVORS;
    private static final DataFlavor[] TEXT_FLAVORS;
    private static final DataFlavor[] RAW_FLAVORS;

    static
    {
      String prefix;

      try
      {
        HTML_FLAVORS = new DataFlavor[3];
        prefix = DATA_FLAVOR_HTML + ";charset=unicode;class=";
        HTML_FLAVORS[0] = new DataFlavor( prefix + "java.lang.String" );
        HTML_FLAVORS[1] = new DataFlavor( prefix + "java.io.Reader" );
        HTML_FLAVORS[2] = new DataFlavor( prefix + "java.io.InputStream" );

        CSV_FLAVORS = new DataFlavor[3];
        prefix = DATA_FLAVOR_CSV + ";charset=unicode;class=";
        CSV_FLAVORS[0] = new DataFlavor( prefix + "java.lang.String" );
        CSV_FLAVORS[1] = new DataFlavor( prefix + "java.io.Reader" );
        CSV_FLAVORS[2] = new DataFlavor( prefix + "java.io.InputStream" );

        TEXT_FLAVORS = new DataFlavor[3];
        prefix = DATA_FLAVOR_TEXT + ";charset=unicode;class=";
        TEXT_FLAVORS[0] = new DataFlavor( prefix + "java.lang.String" );
        TEXT_FLAVORS[1] = new DataFlavor( prefix + "java.io.Reader" );
        TEXT_FLAVORS[2] = new DataFlavor( prefix + "java.io.InputStream" );

        RAW_FLAVORS = new DataFlavor[2];
        prefix = DataFlavor.javaJVMLocalObjectMimeType + ";charset=unicode;class=";
        RAW_FLAVORS[0] = new DataFlavor( prefix + "java.lang.String" );
        RAW_FLAVORS[1] = DataFlavor.stringFlavor;
      }
      catch ( ClassNotFoundException exception )
      {
        throw new RuntimeException( exception );
      }
    }

    // VARIABLES

    private final String textData;
    private final String htmlData;
    private final String rawData;

    // CONSTRUCTORS

    /**
     * Creates a new {@link TableDataTransferable} instance.
     * 
     * @param aTextData
     * @param aHtmlData
     * @param aRawData
     */
    public TableDataTransferable( final String aTextData, final String aHtmlData, final String aRawData )
    {
      this.textData = aTextData;
      this.htmlData = aHtmlData;
      this.rawData = aRawData;
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
      String data = null;
      if ( isHtmlFlavor( aFlavor ) )
      {
        data = getHtmlData();
      }
      else if ( isCsvFlavor( aFlavor ) )
      {
        data = getCsvData();
      }
      else if ( isTextFlavor( aFlavor ) )
      {
        data = getTextData();
      }
      else if ( isRawFlavor( aFlavor ) )
      {
        data = getRawData();
        return data;
      }

      final Class<?> representationClass = aFlavor.getRepresentationClass();

      data = ( data == null ) ? "" : data;
      if ( String.class.equals( representationClass ) )
      {
        return data;
      }
      else if ( Reader.class.equals( representationClass ) )
      {
        return new StringReader( data );
      }
      else if ( InputStream.class.equals( representationClass ) )
      {
        return new ByteArrayInputStream( data.getBytes( "utf-8" ) );
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
      List<DataFlavor> dataFlavors = new ArrayList<DataFlavor>();
      if ( isHtmlSupported() )
      {
        dataFlavors.addAll( Arrays.asList( HTML_FLAVORS ) );
      }
      if ( isCsvSupported() )
      {
        dataFlavors.addAll( Arrays.asList( CSV_FLAVORS ) );
      }
      if ( isTextSupported() )
      {
        dataFlavors.addAll( Arrays.asList( TEXT_FLAVORS ) );
      }
      if ( isRawSupported() )
      {
        dataFlavors.addAll( Arrays.asList( RAW_FLAVORS ) );
      }
      return dataFlavors.toArray( new DataFlavor[dataFlavors.size()] );
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
     * Fetch the data in a text/csv format.
     */
    protected String getCsvData()
    {
      return this.textData.replaceAll( "\t", "," );
    }

    /**
     * Fetch the data in a text/html format
     */
    protected String getHtmlData()
    {
      return this.htmlData;
    }

    /**
     * Fetch the data in a "raw" format.
     */
    protected String getRawData()
    {
      return this.rawData;
    }

    /**
     * Fetch the data in a text/plain format.
     */
    protected String getTextData()
    {
      return this.textData;
    }

    /**
     * Returns whether or not the specified data flavor is an CSV flavor that is
     * supported.
     * 
     * @param aFlavor
     *          the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    protected boolean isCsvFlavor( final DataFlavor aFlavor )
    {
      DataFlavor[] flavors = CSV_FLAVORS;
      return findFlavor( flavors, aFlavor );
    }

    /**
     * Should the CSV flavors be offered? If so, the method getCSVData should be
     * implemented to provide something reasonable.
     */
    protected boolean isCsvSupported()
    {
      return this.textData != null;
    }

    /**
     * Returns whether or not the specified data flavor is an HTML flavor that
     * is supported.
     * 
     * @param aFlavor
     *          the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    protected boolean isHtmlFlavor( final DataFlavor aFlavor )
    {
      DataFlavor[] flavors = HTML_FLAVORS;
      return findFlavor( flavors, aFlavor );
    }

    /**
     * Should the HTML flavors be offered? If so, the method getHTMLData should
     * be implemented to provide something reasonable.
     */
    protected boolean isHtmlSupported()
    {
      return this.htmlData != null;
    }

    /**
     * Returns whether or not the specified data flavor is a String flavor that
     * is supported.
     * 
     * @param aFlavor
     *          the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    protected boolean isRawFlavor( final DataFlavor aFlavor )
    {
      DataFlavor[] flavors = RAW_FLAVORS;
      return findFlavor( flavors, aFlavor );
    }

    /**
     * Should the raw-data flavors be offered? If so, the method getRawData
     * should be implemented to provide something reasonable.
     */
    protected boolean isRawSupported()
    {
      return this.rawData != null;
    }

    /**
     * Returns whether or not the specified data flavor is an plain flavor that
     * is supported.
     * 
     * @param aFlavor
     *          the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    protected boolean isTextFlavor( final DataFlavor aFlavor )
    {
      DataFlavor[] flavors = TEXT_FLAVORS;
      return findFlavor( flavors, aFlavor );
    }

    /**
     * Should the plain text flavors be offered? If so, the method getPlainData
     * should be implemented to provide something reasonable.
     */
    protected boolean isTextSupported()
    {
      return this.textData != null;
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
      if ( !( aComponent instanceof JTable ) )
      {
        return null;
      }

      JTable table = ( JTable )aComponent;
      if ( !table.getRowSelectionAllowed() && !table.getColumnSelectionAllowed() )
      {
        return null;
      }

      int[] rows;
      int[] cols;

      if ( !table.getRowSelectionAllowed() )
      {
        int rowCount = table.getRowCount();

        rows = new int[rowCount];
        for ( int counter = 0; counter < rowCount; counter++ )
        {
          rows[counter] = counter;
        }
      }
      else
      {
        rows = table.getSelectedRows();
      }

      if ( !table.getColumnSelectionAllowed() )
      {
        int colCount = table.getColumnCount();

        cols = new int[colCount];
        for ( int counter = 0; counter < colCount; counter++ )
        {
          cols[counter] = counter;
        }
      }
      else
      {
        cols = table.getSelectedColumns();
      }

      if ( ( rows == null ) || ( cols == null ) || ( rows.length == 0 ) || ( cols.length == 0 ) )
      {
        return null;
      }

      StringBuilder rawBuf = new StringBuilder();
      StringBuilder textBuf = new StringBuilder();
      StringBuilder htmlBuf = new StringBuilder(
          "<html><head><meta charset=\"utf-8\"><meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"></head><body><table>" );

      for ( int row : rows )
      {
        htmlBuf.append( "<tr>" );
        for ( int col : cols )
        {
          Object obj = table.getValueAt( row, col );
          String val = getRenderedValue( table, obj, row, col );

          rawBuf.append( ( ( obj == null ) ? "" : obj.toString() ) ).append( "\t" );
          textBuf.append( val ).append( "\t" );
          htmlBuf.append( "<td>" ).append( val ).append( "</td>" );
        }
        // we want a newline at the end of each line and not a tab
        textBuf.deleteCharAt( textBuf.length() - 1 ).append( "\n" );
        htmlBuf.append( "</tr>" );
      }

      // remove the last newline
      textBuf.deleteCharAt( textBuf.length() - 1 );
      htmlBuf.append( "</table></body></html>" );

      return new TableDataTransferable( textBuf.toString(), htmlBuf.toString(), rawBuf.toString() );
    }

    /**
     * @param aComponent
     * @return
     */
    private String extractTextFromComponent( final Component aComponent )
    {
      String text = "";
      if ( aComponent instanceof JLabel )
      {
        text = ( ( JLabel )aComponent ).getText();
      }
      else if ( aComponent instanceof AbstractButton )
      {
        text = ( ( AbstractButton )aComponent ).getText();
      }
      text = text.replaceFirst( "<html><body>", "" );
      text = text.replaceFirst( "</body></html>", "" );
      return text;
    }

    /**
     * @param aTable
     * @param aValue
     * @param aRow
     * @param aColumn
     * @return
     */
    private String getRenderedValue( final JTable aTable, final Object aValue, final int aRow, final int aColumn )
    {
      TableCellRenderer renderer = aTable.getCellRenderer( aRow, aColumn );
      Component comp = renderer.getTableCellRendererComponent( aTable, aValue, false, false, aRow, aColumn );
      return extractTextFromComponent( comp );
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

    return comp;
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
}
