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
package nl.lxtreme.ols.client.signaldisplay.view;


import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.client.signaldisplay.model.*;
import nl.lxtreme.ols.client.signaldisplay.util.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;
import nl.lxtreme.ols.util.swing.component.JLxTable.TableCellRendererAdapter;


/**
 * Provides a dockable tool-window that presents an overview of the annotations.
 */
public class AnnotationOverview extends AbstractToolWindow implements ExportAnnotationsAction.ExportDataProvider,
    IDataModelChangeListener, IAnnotationDataChangedListener
{
  // INNER TYPES

  /**
   * Provides a cell renderer adapter for rendering the background using the
   * color provided by data annotations.
   */
  private static class AnnotationCellRenderer implements TableCellRendererAdapter
  {
    // VARIABLES

    private Font font; // cache value

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Component render( final Component aCellComponent, final Object aCellValue, final boolean aIsSelected )
    {
      if ( this.font == null )
      {
        Font f = aCellComponent.getFont();
        this.font = new Font( Font.MONOSPACED, f.getStyle(), f.getSize() );
      }
      aCellComponent.setFont( this.font );

      if ( aIsSelected )
      {
        return aCellComponent;
      }

      if ( aCellValue instanceof DataAnnotation )
      {
        Object val = ( ( DataAnnotation )aCellValue ).getProperties().get( DataAnnotation.KEY_COLOR );
        if ( val instanceof String )
        {
          aCellComponent.setBackground( ColorUtils.parseColor( ( String )val ) );
        }
        else if ( val instanceof Color )
        {
          aCellComponent.setBackground( ( Color )val );
        }
      }

      return aCellComponent;
    }
  }

  /**
   * Provides a customer key for use in a {@link Map}.
   */
  private static class AnnotationKey implements Comparable<AnnotationKey>
  {
    // VARIABLES

    final long startTime;
    final long endTime;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationKey} instance.
     */
    public AnnotationKey( final DataAnnotation aAnnotation )
    {
      this( aAnnotation.getStartTimestamp(), aAnnotation.getEndTimestamp() );
    }

    /**
     * Creates a new {@link AnnotationKey} instance.
     */
    AnnotationKey( final long aStartTime, final long aEndTime )
    {
      this.startTime = aStartTime;
      this.endTime = aEndTime;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo( final AnnotationKey aOtherKey )
    {
      int result = ( int )( this.startTime - aOtherKey.startTime );
      if ( result == 0 )
      {
        result = ( int )( aOtherKey.endTime - this.endTime );
      }
      return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals( final Object obj )
    {
      if ( this == obj )
      {
        return true;
      }
      if ( ( obj == null ) || !( obj instanceof AnnotationKey ) )
      {
        return false;
      }

      final AnnotationKey other = ( AnnotationKey )obj;
      if ( ( this.endTime != other.endTime ) || ( this.startTime != other.startTime ) )
      {
        return false;
      }

      return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = ( prime * result ) + ( int )( this.endTime ^ ( this.endTime >>> 32 ) );
      result = ( prime * result ) + ( int )( this.startTime ^ ( this.startTime >>> 32 ) );
      return result;
    }
  }

  /**
   * 
   */
  private static class AnnotationsTable extends JLxTable
  {
    // CONSTANTS

    private static final DataFlavor[] HTML_FLAVORS;
    private static final DataFlavor[] CSV_FLAVORS;
    private static final DataFlavor[] TEXT_FLAVORS;
    private static final DataFlavor[] RAW_FLAVORS;

    private static final long serialVersionUID = 1L;

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

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationsTable} instance.
     */
    public AnnotationsTable( final AbstractTableModel aTableModel )
    {
      super( aTableModel, ( TableColumnModel )aTableModel );

      setCellRendererAdapter( new AnnotationCellRenderer() );
      setSelectionMode( ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
      setAutoResizeMode( JTable.AUTO_RESIZE_OFF );
      setIntercellSpacing( new Dimension( 2, 2 ) );
      setAutoCreateColumnsFromModel( false );
      setColumnSelectionAllowed( true );
      setAutoCreateRowSorter( false );
      setFillsViewportHeight( true );
      setRowSelectionAllowed( true );
      setShowGrid( false );

      setDefaultRenderer( DataAnnotation.class, new DataAnnotationCellRenderer() );
      setDefaultRenderer( Double.class, new TimeCellRenderer() );
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    protected Object getTableData( final DataFlavor aFlavor ) throws UnsupportedFlavorException, IOException
    {
      int[] rows = getSelectedRows();
      int[] cols = getSelectedColumns();

      if ( ( rows.length == 0 ) || !getRowSelectionAllowed() )
      {
        int rowCount = getRowCount();

        rows = new int[rowCount];
        for ( int counter = 0; counter < rowCount; counter++ )
        {
          rows[counter] = counter;
        }
      }

      if ( ( cols.length == 0 ) || !getColumnSelectionAllowed() )
      {
        int colCount = getColumnCount();

        cols = new int[colCount];
        for ( int counter = 0; counter < colCount; counter++ )
        {
          cols[counter] = counter;
        }
      }

      if ( ( rows.length == 0 ) || ( cols.length == 0 ) )
      {
        return null;
      }

      String mimeType = aFlavor.getPrimaryType() + "/" + aFlavor.getSubType();
      if ( DATA_FLAVOR_HTML.equals( mimeType ) )
      {
        return getTableDataAsHtml( rows, cols );
      }
      else if ( DATA_FLAVOR_CSV.equals( mimeType ) )
      {
        return getTableDataAsCsv( rows, cols );
      }
      else if ( DATA_FLAVOR_TEXT.equals( mimeType ) )
      {
        return getTableDataAsText( rows, cols );
      }

      return super.getTableData( aFlavor );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected DataFlavor[] getTableDataFlavors()
    {
      List<DataFlavor> result = new ArrayList<DataFlavor>();
      result.addAll( Arrays.asList( CSV_FLAVORS ) );
      result.addAll( Arrays.asList( HTML_FLAVORS ) );
      result.addAll( Arrays.asList( TEXT_FLAVORS ) );
      result.addAll( Arrays.asList( RAW_FLAVORS ) );
      return result.toArray( new DataFlavor[result.size()] );
    }

    /**
     * @param aComponent
     * @return
     */
    private String extractHtmlFromComponent( final Component aComponent )
    {
      String text = "";
      if ( aComponent instanceof JLabel )
      {
        JLabel label = ( JLabel )aComponent;
        text = label.getToolTipText();
        if ( ( text == null ) || "".equals( text.trim() ) )
        {
          text = label.getText();
        }
      }
      else if ( aComponent instanceof AbstractButton )
      {
        AbstractButton button = ( AbstractButton )aComponent;
        text = button.getToolTipText();
        if ( ( text == null ) || "".equals( text.trim() ) )
        {
          text = button.getText();
        }
      }
      return text;
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
        JLabel label = ( JLabel )aComponent;
        text = label.getText();
      }
      else if ( aComponent instanceof AbstractButton )
      {
        AbstractButton button = ( AbstractButton )aComponent;
        text = button.getText();
      }
      return text;
    }

    /**
     * @param aTable
     * @param aValue
     * @param aRow
     * @param aColumn
     * @return
     */
    private String getRenderedValueAsHtml( final JTable aTable, final Object aValue, final int aRow, final int aColumn )
    {
      TableCellRenderer renderer = aTable.getCellRenderer( aRow, aColumn );
      Component comp = renderer.getTableCellRendererComponent( aTable, aValue, false, false, aRow, aColumn );
      String text = extractHtmlFromComponent( comp );
      text = text.replaceAll( "<html>.*<body>(.*)</body></html>", "$1" );
      return text;
    }

    /**
     * @param aTable
     * @param aValue
     * @param aRow
     * @param aColumn
     * @return
     */
    private String getRenderedValueAsText( final JTable aTable, final Object aValue, final int aRow, final int aColumn )
    {
      TableCellRenderer renderer = aTable.getCellRenderer( aRow, aColumn );
      Component comp = renderer.getTableCellRendererComponent( aTable, aValue, false, false, aRow, aColumn );
      String text = extractTextFromComponent( comp );
      text = text.replaceAll( "<br\\s*/?>", "\n" );
      text = text.replaceAll( "<html>.*<body>(.*)</body></html>", "$1" );
      return text.replaceAll( "<[^>]+>", "" );
    }

    /**
     * @param aRowIndices
     * @param aColumnIndices
     * @return
     */
    private Object getTableDataAsCsv( final int[] aRowIndices, final int[] aColumnIndices )
    {
      return getTableDataAsText( aRowIndices, aColumnIndices, ',', true /* quote */);
    }

    /**
     * @param aRowIndices
     * @param aColumnIndices
     * @return
     */
    private Object getTableDataAsHtml( final int[] aRowIndices, final int[] aColumnIndices )
    {
      StringBuilder buf = new StringBuilder( "<html><head><meta charset=\"utf-8\">" );
      // @formatter:off
      buf.append( "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"><style type=\"text/css\">" )
          .append( "body{margin:2em 4em;padding:0}" )
          .append(
              "body,td,th{font-family:'Avenir Next',Helvetica,Tahoma,Arial,sans-serif;font-size:13px;line-height:1.2em}" )
          .append( "h1{font-size:3.375em}" ).append( "h2{font-size:2.925em}" ).append( "h3{font-size:2.25em}" )
          .append( "h4{font-size:1.95em}" ).append( "h5{font-size:1.5em}" ).append( "h6{font-size:1.3em}" )
          .append( "table{border-collapse:separate}" ).append( "table.main{width:80em}" )
          .append( "th,td{text-align:left;vertical-align:baseline;margin:0;padding:0}" )
          .append( "thead tr{background-color:#26537c;color:#FFF}" )
          .append( "tr.oe{border-top:solid 1px rgba(12,93,165,0.1);border-bottom:solid 1px rgba(12,93,165,0.1)}" )
          .append( "tr.oe:nth-child(odd){background-color:rgba(255,255,255,0.75)}" )
          .append( "tr.oe:nth-child(even){background-color:rgba(124,189,248,0.6)}" ).append( "col.col1{width:3%}" )
          .append( "col:nth-child(even){background-color:rgba(124,189,248,0.3)}" ).append( ".pad{padding:.5em}" )
          .append( "table.desc{margin-left:-.25em;width:100%}" ).append( ".desc th,.desc td{padding:.25em}" )
          .append( "table.desc tr > th{width:4em}" )
          .append( "table.desc table[border=0] tr:nth-child(odd){background-color:rgba(124,189,248,0.2)}" )
          .append( "col.col2,col.col3{width:7%}" ).append( "</style></head><body>" );
      // @formatter:on

      buf.append( "<table class='main'><colgroup>" );
      for ( int i = 0; i < aColumnIndices.length; i++ )
      {
        buf.append( "<col class=\"col" ).append( i + 1 ).append( "\">" );
      }
      buf.append( "</colgroup><thead><tr>" );
      for ( int col : aColumnIndices )
      {
        String columnName = getColumnName( col );
        buf.append( "<th class=\"pad\">" ).append( columnName ).append( "</th>" );
      }
      buf.append( "</tr></thead>" );

      buf.append( "<tbody>" );
      for ( int row : aRowIndices )
      {
        buf.append( "<tr class='oe'>" );
        for ( int col : aColumnIndices )
        {
          Object obj = getValueAt( row, col );
          String val = getRenderedValueAsHtml( this, obj, row, col );

          buf.append( "<td class=\"pad\">" ).append( val ).append( "</td>" );
        }
        buf.append( "</tr>" );
      }
      buf.append( "</tbody>" );

      buf.append( "</table></body></html>" );

      return buf.toString();
    }

    /**
     * @param aRowIndices
     * @param aColumnIndices
     * @return
     */
    private Object getTableDataAsText( final int[] aRowIndices, final int[] aColumnIndices )
    {
      return getTableDataAsText( aRowIndices, aColumnIndices, '\t', false /* quote */);
    }

    /**
     * @param aRowIndices
     * @param aColumnIndices
     * @return
     */
    private Object getTableDataAsText( final int[] aRowIndices, final int[] aColumnIndices, final char aSeparator,
        final boolean aQuoteValues )
    {
      // Plain text
      StringBuilder buf = new StringBuilder();

      for ( int row : aRowIndices )
      {
        for ( int col : aColumnIndices )
        {
          Object obj = getValueAt( row, col );
          String val = getRenderedValueAsText( this, obj, row, col );

          buf.append( aQuoteValues ? quote( val ) : val ).append( aSeparator );
        }
        // we want a newline at the end of each line and not a tab
        buf.deleteCharAt( buf.length() - 1 ).append( "\n" );
      }

      // remove the last newline
      buf.deleteCharAt( buf.length() - 1 );

      return buf.toString();
    }

    /**
     * @param aValue
     * @return
     */
    private String quote( final Object aValue )
    {
      final String value;
      if ( aValue == null )
      {
        value = "";
      }
      else
      {
        if ( aValue instanceof Character )
        {
          if ( Character.isLetterOrDigit( ( ( Character )aValue ).charValue() ) )
          {
            value = String.valueOf( aValue );
          }
          else
          {
            value = "";
          }
        }
        else
        {
          value = String.valueOf( aValue );
        }
      }
      return "\"" + value + "\"";
    }
  }

  /**
   * Provides a container for annotations that also acts as a table model.
   */
  private static class AnnotationTableModel extends AbstractTableModel implements TableColumnModel,
      PropertyChangeListener
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    private static final String COL_ID = "id";
    private static final String COL_START_TIME = "startTime";
    private static final String COL_END_TIME = "endTime";

    // VARIABLES

    private final SignalDiagramController controller;
    private final AtomicReference<DataHolder> dataRef;

    private volatile ListSelectionModel listSelectionModel;
    private volatile int columnMargin;
    private volatile int totalColumnWidth;

    // CONSTRUCTORS

    /**
     * Creates a new {@link AnnotationTableModel} instance.
     */
    public AnnotationTableModel( final SignalDiagramController aController )
    {
      this.controller = aController;
      this.dataRef = new AtomicReference<DataHolder>( new DataHolder( createDefaultColumns() ) );
      this.listSelectionModel = new DefaultListSelectionModel();
    }

    // METHODS

    /**
     * @return
     */
    private static TableColumn[] createDefaultColumns()
    {
      return createDefaultColumns( new TableColumn[3] );
    }

    /**
     * @return
     */
    private static TableColumn[] createDefaultColumns( final TableColumn[] aColumns )
    {
      assert aColumns.length >= 3 : "Too little columns!";
      aColumns[0] = new TableColumn( 0, 50 );
      aColumns[0].setIdentifier( COL_ID );
      aColumns[0].setHeaderValue( "#" );
      aColumns[0].setPreferredWidth( 50 );
      aColumns[1] = new TableColumn( 1, 100 );
      aColumns[1].setIdentifier( COL_START_TIME );
      aColumns[1].setHeaderValue( "Start" );
      aColumns[1].setPreferredWidth( 100 );
      aColumns[2] = new TableColumn( 2, 100 );
      aColumns[2].setIdentifier( COL_END_TIME );
      aColumns[2].setHeaderValue( "End" );
      aColumns[2].setPreferredWidth( 100 );
      return aColumns;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addColumn( final TableColumn aColumn )
    {
      throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addColumnModelListener( final TableColumnModelListener aListener )
    {
      this.listenerList.add( TableColumnModelListener.class, aListener );
    }

    /**
     * Clears this structure, removing all of its data.
     */
    public void clearStructure( final Integer aChannelIdx )
    {
      if ( aChannelIdx == null )
      {
        setDataHolder( new DataHolder( createDefaultColumns() ) );

        fireTableStructureChanged();
      }
      else
      {
        updateStructure();
      }
    }

    /**
     * Returns the data annotation for the row identified by the given index.
     * 
     * @param aRowIndex
     *          the row index of the row to return the data annotation for.
     * @return a data annotation, can be <code>null</code>.
     */
    public DataAnnotation getAnnotation( final int aRowIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      final int colCount = dataHolder.columns.length;
      if ( colCount < 1 )
      {
        return null;
      }

      for ( int i = Math.min( 3, colCount - 1 ); i < colCount; i++ )
      {
        Object[] row = dataHolder.data[aRowIndex];
        if ( row.length <= i )
        {
          continue;
        }

        Object value = row[i];
        if ( value instanceof DataAnnotation )
        {
          return ( DataAnnotation )value;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TableColumn getColumn( final int aColumnIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      return dataHolder.columns[aColumnIndex];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<?> getColumnClass( final int aColumnIndex )
    {
      switch ( aColumnIndex )
      {
        case 0:
          return Integer.class;

        case 1:
        case 2:
          return Double.class;

        default:
          return DataAnnotation.class;
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnCount()
    {
      final DataHolder dataHolder = getDataHolder();
      return dataHolder.columns.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnIndex( final Object aColumnIdentifier )
    {
      final DataHolder dataHolder = getDataHolder();

      int l = dataHolder.columns.length;
      for ( int i = 0; i < l; i++ )
      {
        TableColumn column = dataHolder.columns[i];
        if ( column.getIdentifier().equals( aColumnIdentifier ) )
        {
          return i;
        }
      }
      throw new IllegalArgumentException( "Identifier not found" );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnIndexAtX( final int aXPosition )
    {
      final DataHolder dataHolder = getDataHolder();

      int l = dataHolder.columns.length;
      int w = aXPosition;
      for ( int i = 0; i < l; i++ )
      {
        TableColumn column = dataHolder.columns[i];

        w = w - column.getWidth();
        if ( w < 0 )
        {
          return i;
        }
      }
      return -1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnMargin()
    {
      return this.columnMargin;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getColumnName( final int aColumn )
    {
      final DataHolder dataHolder = getDataHolder();
      return String.valueOf( dataHolder.columns[aColumn].getHeaderValue() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Enumeration<TableColumn> getColumns()
    {
      final DataHolder dataHolder = getDataHolder();
      return new Enumeration<TableColumn>()
      {
        private final TableColumn[] columns = dataHolder.columns;
        private volatile int idx = 0;

        @Override
        public boolean hasMoreElements()
        {
          return this.idx < this.columns.length;
        }

        @Override
        public TableColumn nextElement()
        {
          if ( this.idx >= this.columns.length )
          {
            throw new NoSuchElementException();
          }
          return this.columns[this.idx++];
        }
      };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getColumnSelectionAllowed()
    {
      return false; // never allowed
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRowCount()
    {
      final DataHolder dataHolder = getDataHolder();
      if ( dataHolder == null )
      {
        return 0;
      }

      return dataHolder.data.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSelectedColumnCount()
    {
      return 0; // always zero selected
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] getSelectedColumns()
    {
      return new int[0]; // no selection
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ListSelectionModel getSelectionModel()
    {
      return this.listSelectionModel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getTotalColumnWidth()
    {
      if ( this.totalColumnWidth < 0 )
      {
        return calculateTotalColumnWidth();
      }
      return this.totalColumnWidth;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getValueAt( final int aRowIndex, final int aColumnIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      if ( dataHolder == null )
      {
        return null;
      }

      return dataHolder.data[aRowIndex][aColumnIndex];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void moveColumn( final int aColumnIndex, final int aNewIndex )
    {
      final DataHolder dataHolder = getDataHolder();
      final TableColumn[] columns = dataHolder.columns;
      final int columnCount = columns.length;

      if ( ( aColumnIndex < 0 ) || ( aColumnIndex >= columnCount ) || ( aNewIndex < 0 ) || ( aNewIndex >= columnCount ) )
      {
        throw new IllegalArgumentException( "moveColumn() - Index out of range" );
      }

      // If the column has not yet moved far enough to change positions
      // post the event anyway, the "draggedDistance" property of the
      // tableHeader will say how far the column has been dragged.
      // Here we are really trying to get the best out of an
      // API that could do with some rethinking. We preserve backward
      // compatibility by slightly bending the meaning of these methods.
      if ( aColumnIndex == aNewIndex )
      {
        fireColumnMoved( new TableColumnModelEvent( this, aColumnIndex, aNewIndex ) );
        return;
      }

      final TableColumn moved = columns[aColumnIndex];
      // Delete element from array...
      System.arraycopy( columns, aColumnIndex + 1, columns, aColumnIndex, columnCount - 1 - aColumnIndex );
      // Make space for new element...
      System.arraycopy( columns, aNewIndex, columns, aNewIndex + 1, columnCount - 1 - aNewIndex );
      // Set actual (inserted) element...
      columns[aNewIndex] = moved;

      boolean selected = this.listSelectionModel.isSelectedIndex( aColumnIndex );
      this.listSelectionModel.removeIndexInterval( aColumnIndex, aColumnIndex );
      this.listSelectionModel.insertIndexInterval( aNewIndex, 1, true );

      if ( selected )
      {
        this.listSelectionModel.addSelectionInterval( aNewIndex, aNewIndex );
      }
      else
      {
        this.listSelectionModel.removeSelectionInterval( aNewIndex, aNewIndex );
      }

      fireColumnMoved( new TableColumnModelEvent( this, aColumnIndex, aNewIndex ) );
    }

    /**
     * {@inheritDoc}
     */
    public void propertyChange( final PropertyChangeEvent aEvent )
    {
      String name = aEvent.getPropertyName();
      if ( ( name == "width" ) || ( name == "preferredWidth" ) )
      {
        invalidateWidthCache();
        // This is a misnomer, we're using this method
        // simply to cause a relayout.
        fireColumnMarginChanged();
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeColumn( final TableColumn aColumn )
    {
      throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeColumnModelListener( final TableColumnModelListener aListener )
    {
      this.listenerList.remove( TableColumnModelListener.class, aListener );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColumnMargin( final int aNewMargin )
    {
      this.columnMargin = aNewMargin;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setColumnSelectionAllowed( final boolean aFlag )
    {
      // Ignore; column selection is not allowed...
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectionModel( final ListSelectionModel aNewModel )
    {
      this.listSelectionModel = aNewModel;
    }

    /**
     * Updates the structure of this table, which is a <em>heavy</em> operation
     * and should be called with care!
     */
    public void updateStructure()
    {
      final AnnotationData data = getModel().getAnnotationData();
      final SortedSet<Annotation> annotations = data.getAnnotations();

      SortedMap<AnnotationKey, List<DataAnnotation>> dataAnnotations = new TreeMap<AnnotationKey, List<DataAnnotation>>();
      Map<Integer, String> channelNames = new HashMap<Integer, String>();

      // Step 1: determine dimensions of the table...
      for ( Annotation annotation : annotations )
      {
        final Integer channelIdx = Integer.valueOf( annotation.getChannelIndex() );

        if ( annotation instanceof DataAnnotation )
        {
          if ( !channelNames.containsKey( channelIdx ) )
          {
            channelNames.put( channelIdx, null );
          }

          final DataAnnotation dataAnnotation = ( DataAnnotation )annotation;

          AnnotationKey key = new AnnotationKey( dataAnnotation );
          List<DataAnnotation> values = dataAnnotations.get( key );
          if ( values == null )
          {
            values = new ArrayList<DataAnnotation>();
            dataAnnotations.put( key, values );
          }
          values.add( dataAnnotation );
        }
        else if ( annotation instanceof LabelAnnotation )
        {
          channelNames.put( channelIdx, ( ( LabelAnnotation )annotation ).getData() );
        }
        else
        {
          System.out.println( "Ignoring annotation: " + annotation );
        }
      }

      // Step 2: remove empty columns...
      SortedSet<Integer> seenChannels = new TreeSet<Integer>();
      for ( List<DataAnnotation> entry : dataAnnotations.values() )
      {
        for ( DataAnnotation annotation : entry )
        {
          seenChannels.add( Integer.valueOf( annotation.getChannelIndex() ) );
        }
      }

      // Step 3: normalize column indices...
      SortedMap<Integer, Integer> columnIndices = new TreeMap<Integer, Integer>();
      int columnIdx = 0;
      for ( Integer channelIdx : seenChannels )
      {
        columnIndices.put( channelIdx, Integer.valueOf( columnIdx++ ) );
      }

      // Step 4: create data structure...
      final int columnCount = 3 + columnIndices.size();
      Object[][] newData = new Object[dataAnnotations.size()][columnCount];

      final long triggerPos = getTriggerPosition();
      final double sampleRate = getSampleRate();

      // Step 5: fill data structure...
      int row = 0;
      for ( Map.Entry<AnnotationKey, List<DataAnnotation>> entry : dataAnnotations.entrySet() )
      {
        AnnotationKey key = entry.getKey();

        Object[] columnData = new Object[columnCount];

        columnData[0] = Integer.valueOf( row );
        columnData[1] = Double.valueOf( ( key.startTime - triggerPos ) / sampleRate );
        columnData[2] = Double.valueOf( ( key.endTime - triggerPos ) / sampleRate );
        for ( DataAnnotation annotation : entry.getValue() )
        {
          int idx = columnIndices.get( Integer.valueOf( annotation.getChannelIndex() ) ).intValue();
          columnData[idx + 3] = annotation;
        }
        newData[row++] = columnData;
      }

      // Step 6: create column headers...
      TableColumn[] columns = new TableColumn[columnCount];
      createDefaultColumns( columns );
      columnIdx = 3;
      for ( Integer channelIdx : seenChannels )
      {
        TableColumn column = new TableColumn( columnIdx );
        column.setIdentifier( String.format( "col-%d", Integer.valueOf( columnIdx ) ) );
        column.setHeaderValue( channelNames.get( channelIdx ) );
        column.setPreferredWidth( 300 );

        columns[columnIdx] = column;
        columnIdx++;
      }

      // Swap...
      setDataHolder( new DataHolder( newData, columns ) );

      fireTableStructureChanged();
    }

    /**
     * Notifies all listeners that have registered interest for notification on
     * this event type. The event instance is lazily created using the
     * parameters passed into the fire method.
     * 
     * @see EventListenerList
     */
    protected void fireColumnMarginChanged()
    {
      ChangeEvent changeEvent = null;

      TableColumnModelListener[] listeners = this.listenerList.getListeners( TableColumnModelListener.class );
      for ( int i = listeners.length - 1; i >= 0; i-- )
      {
        TableColumnModelListener listener = listeners[i];
        if ( changeEvent == null )
        {
          changeEvent = new ChangeEvent( this );
        }
        listener.columnMarginChanged( changeEvent );
      }
    }

    /**
     * Notifies all listeners that have registered interest for notification on
     * this event type. The event instance is lazily created using the
     * parameters passed into the fire method.
     * 
     * @param aEvent
     *          the event received
     * @see EventListenerList
     */
    protected void fireColumnMoved( final TableColumnModelEvent aEvent )
    {
      TableColumnModelListener[] listeners = this.listenerList.getListeners( TableColumnModelListener.class );
      for ( int i = listeners.length - 1; i >= 0; i-- )
      {
        listeners[i].columnMoved( aEvent );
      }
    }

    /**
     * Calculates the total column width.
     */
    private int calculateTotalColumnWidth()
    {
      DataHolder data = getDataHolder();

      int width = 0;
      for ( TableColumn column : data.columns )
      {
        width += column.getWidth();
      }
      return this.totalColumnWidth = width;
    }

    /**
     * @return
     */
    private DataHolder getDataHolder()
    {
      return this.dataRef.get();
    }

    /**
     * @return
     */
    private SignalDiagramModel getModel()
    {
      return this.controller.getSignalDiagramModel();
    }

    /**
     * @return
     */
    private double getSampleRate()
    {
      return getModel().getSampleRate();
    }

    /**
     * @return
     */
    private long getTriggerPosition()
    {
      final Long result = getModel().getTriggerPosition();
      if ( result == null )
      {
        return 0L;
      }
      return result.longValue();
    }

    /**
     * Invalidates the cached total column-width, causing it to be recalculated
     * on request.
     */
    private void invalidateWidthCache()
    {
      this.totalColumnWidth = -1;
    }

    /**
     * @param aDataHolder
     */
    private void setDataHolder( final DataHolder aDataHolder )
    {
      assert aDataHolder != null : "Data Holder cannot be null!";
      for ( TableColumn column : aDataHolder.columns )
      {
        column.addPropertyChangeListener( this );
      }

      DataHolder old;
      do
      {
        old = this.dataRef.get();
      }
      while ( !this.dataRef.compareAndSet( old, aDataHolder ) );

      if ( old != null )
      {
        for ( TableColumn column : old.columns )
        {
          column.removePropertyChangeListener( this );
        }
      }
    }
  }

  /**
   * Provides a table cell renderer for {@link DataAnnotation}s.
   */
  private static class DataAnnotationCellRenderer extends DefaultTableCellRenderer
  {
    private static final long serialVersionUID = 1L;

    private final AnnotationHelper annotationHelper;

    /**
     * Creates a new {@link DataAnnotationCellRenderer} instance.
     */
    public DataAnnotationCellRenderer()
    {
      this.annotationHelper = new AnnotationHelper();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Component getTableCellRendererComponent( final JTable aTable, final Object aValue,
        final boolean aIsSelected, final boolean aHasFocus, final int aRow, final int aColumn )
    {
      String text = "";
      String tooltip = "";

      final DataAnnotation annotation = ( DataAnnotation )aValue;

      if ( annotation != null )
      {
        text = this.annotationHelper.getText( annotation );
        tooltip = this.annotationHelper.getDescription( null, annotation, false /* aIncludeTimingData */);
      }

      JLabel result = ( JLabel )super.getTableCellRendererComponent( aTable, text, aIsSelected, aHasFocus, aRow,
          aColumn );
      if ( !"".equals( tooltip ) )
      {
        result.setToolTipText( tooltip );
      }

      // Make sure the row height is sufficient to fill all text...
      int prefHeight = getPreferredSize().height;
      if ( aTable.getRowHeight( aRow ) < prefHeight )
      {
        aTable.setRowHeight( aRow, prefHeight );
      }

      return result;
    }
  }

  /**
   * Data holder for the {@link AnnotationTableModel}.
   */
  private static class DataHolder
  {
    // VARIABLES

    final Object[][] data;
    final TableColumn[] columns;

    // CONSTRUCTORS

    /**
     * Creates a new {@link DataHolder} instance.
     */
    public DataHolder( final Object[][] aData, final TableColumn[] aColumns )
    {
      this.data = aData;
      this.columns = aColumns;
    }

    /**
     * Creates a new {@link DataHolder} instance.
     */
    public DataHolder( final TableColumn[] aColumns )
    {
      this.data = new Object[0][0];
      this.columns = aColumns;
    }
  }

  /**
   * Proivdes a table cell renderer for relative time.
   */
  private static class TimeCellRenderer extends DefaultTableCellRenderer
  {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getTableCellRendererComponent( final JTable aTable, final Object aValue,
        final boolean aIsSelected, final boolean aHasFocus, final int aRow, final int aColumn )
    {
      Object value = aValue;
      if ( value instanceof Double )
      {
        value = Unit.Time.format( ( ( Double )value ).doubleValue() );
      }

      JLabel label = ( JLabel )super.getTableCellRendererComponent( aTable, value, aIsSelected, //
          aHasFocus, aRow, aColumn );
      label.setHorizontalAlignment( SwingConstants.RIGHT );

      return label;
    }
  }

  // CONSTANTS

  /** The identifier of this tool-window view. */
  public static final String ID = "Annotations";

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final AnnotationTableModel container;
  private final ExportAnnotationsAction exportAction;
  private JLxTable table;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AnnotationOverview} instance.
   */
  public AnnotationOverview( final SignalDiagramController aController )
  {
    super( ID, aController );

    this.container = new AnnotationTableModel( aController );
    this.exportAction = new ExportAnnotationsAction( this );
  }

  // METHODS

  /**
   * Factory method to create a new {@link AnnotationOverview} instance.
   * 
   * @param aController
   *          the controller to use for the SignalDetailsView instance, cannot
   *          be <code>null</code>.
   * @return a new {@link AnnotationOverview} instance, never <code>null</code>.
   */
  public static AnnotationOverview create( final SignalDiagramController aController )
  {
    final AnnotationOverview result = new AnnotationOverview( aController );

    aController.addAnnotationDataChangedListener( result );
    aController.addDataModelChangeListener( result );

    result.initComponent();

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void annotationDataChanged( final AnnotationData aData )
  {
    this.container.updateStructure();
    updateButtonState();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void annotationDataCleared( final Integer aChannelIdx )
  {
    this.container.clearStructure( aChannelIdx );
    updateButtonState();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void dataModelChanged( final AcquisitionData aData )
  {
    this.container.updateStructure();
    updateButtonState();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public JLxTable getTable()
  {
    return this.table;
  }

  /**
   * Returns the action responsible for exporting the contents of this overview
   * to a file.
   * 
   * @return the export action, never <code>null</code>.
   */
  public ExportAnnotationsAction getExportAction()
  {
    return this.exportAction;
  }

  /**
   * Jumps to the given annotation in the current signal diagram.
   * 
   * @param aAnnotation
   *          the annotation to jump to, cannot be <code>null</code>.
   */
  final void jumpTo( final DataAnnotation aAnnotation )
  {
    long start = aAnnotation.getStartTimestamp();
    long end = aAnnotation.getEndTimestamp();

    // determine the middle of the annotation...
    long ptr = start + ( ( end - start ) / 2 );

    getController().scrollToTimestamp( ptr );
  }

  /**
   * Initializes this component.
   */
  private void initComponent()
  {
    setOpaque( false );
    setLayout( new BorderLayout() );
    setName( "Annotations" );

    this.table = new AnnotationsTable( this.container );
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
              jumpTo( annotation );
            }
          }
        }
      }
    } );

    add( new JScrollPane( this.table ), BorderLayout.CENTER );

    // Update the structure with the current session data...
    this.container.updateStructure();

    updateButtonState();
  }

  /**
   * Updates the export button state.
   */
  private void updateButtonState()
  {
    final int rowCount = this.table.getModel().getRowCount();
    this.exportAction.setEnabled( rowCount > 0 );
  }
}
