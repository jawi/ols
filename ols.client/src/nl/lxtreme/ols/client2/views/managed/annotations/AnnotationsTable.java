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


import java.awt.*;
import java.awt.datatransfer.*;
import java.io.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.table.*;

import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.component.*;


/**
 * 
 */
class AnnotationsTable extends JLxTable
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
  public AnnotationsTable()
  {
    super();

    setCellRendererAdapter( new AnnotationCellRenderer() );
    setSelectionMode( ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
    setAutoResizeMode( JTable.AUTO_RESIZE_LAST_COLUMN );
    setIntercellSpacing( new Dimension( 2, 2 ) );
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
