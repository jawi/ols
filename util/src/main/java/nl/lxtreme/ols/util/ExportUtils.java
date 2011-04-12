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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.util;


import java.io.*;
import java.util.*;

import nl.lxtreme.ols.util.export.*;


/**
 * Provides several export utilities, like exporters for CSV and/or HTML.
 */
public final class ExportUtils
{
  // INNER TYPES

  /**
   * Provides a simple CSV data exporter, which is basically a set of (optional)
   * headers at the first line followed by a number of comma-separated values.
   */
  public interface CsvExporter
  {
    // METHODS

    /**
     * Adds a new row with cell values to the CSV output.
     * 
     * @param aValues
     *          the cell values to write in this row.
     * @throws IOException
     *           in case of I/O problems.
     */
    void addRow( final Object... aValues ) throws IOException;

    /**
     * Closes this exporter.
     * 
     * @throws IOException
     *           in case of I/O problems.
     */
    void close() throws IOException;

    /**
     * Sets the headers of the CSV file.
     * 
     * @param aHeaders
     *          the array of header names.
     * @throws IOException
     *           in case of I/O problems.
     */
    void setHeaders( final String... aHeaders ) throws IOException;
  }

  /**
   * Provides a simple HTML exporter, which is kind of a tree-structure of
   * HTML-elements (containing attributes).
   */
  public interface HtmlExporter
  {
    // INNER TYPES

    /**
     * Denotes a HTML attribute, or name/value pair.
     */
    public static interface Attribute extends Cloneable
    {
      // METHODS

      /**
       * Creates a deep copy of this attribute.
       * 
       * @see java.lang.Object#clone()
       */
      Attribute clone();

      /**
       * Returns the name part of this attribute.
       * 
       * @return the name, never <code>null</code>.
       */
      String getName();

      /**
       * Returns the value part of this attribute.
       * 
       * @return the value, never <code>null</code>.
       */
      String getValue();

      /**
       * Returns the string representation of this HTML-attribute.
       * 
       * @param aResolver
       *          the macro resolver to use for any found macros.
       * @return the string representation of this attribute, never
       *         <code>null</code>.
       */
      String toString( final MacroResolver aResolver );
    }

    /**
     * Denotes a HTML element.
     */
    public static interface Element extends Cloneable
    {
      // METHODS

      /**
       * Adds an attribute to this element.
       * 
       * @param aName
       *          the name of the attribute to add;
       * @param aValue
       *          the value of the attribute to add.
       * @return this element.
       */
      Element addAttribute( final String aName, final String aValue );

      /**
       * Adds a new child element to this element.
       * 
       * @param aChild
       *          the child element to add, cannot be <code>null</code>.
       * @return the added child element (which is a clone of the one given!).
       */
      Element addChild( final Element aChild );

      /**
       * Adds a new child element to this element.
       * 
       * @param aName
       *          the name of the child element to add, cannot be
       *          <code>null</code>.
       * @return the added child element.
       */
      Element addChild( final String aName );

      /**
       * Adds text content to this element.
       * 
       * @param aValues
       *          the text-values to add, cannot be <code>null</code>.
       * @return this element.
       */
      Element addContent( final String... aValues );

      /**
       * Creates a deep copy of this element.
       * 
       * @see java.lang.Object#clone()
       */
      Element clone();

      /**
       * Returns the attributes of this element.
       * 
       * @return a collection of attributes, never <code>null</code>.
       */
      Collection<Attribute> getAttributes();

      /**
       * Returns the first child with the given name.
       * 
       * @param aName
       *          the name of the child to get, cannot be <code>null</code>.
       * @return the first child with the given name, or <code>null</code> if no
       *         such child was found.
       */
      Element getChildByName( final String aName );

      /**
       * Returns the child-elements of this element.
       * 
       * @return a collection of child-elements, never <code>null</code>.
       */
      Collection<Element> getChildren();

      /**
       * Returns the name of this element.
       * 
       * @return the name of this element, never <code>null</code>.
       */
      String getName();

      /**
       * Returns the string representation of this HTML-element.
       * 
       * @param aResolver
       *          the macro resolver to use for any found macros.
       * @return the string representation of this element, never
       *         <code>null</code>.
       */
      String toString( final MacroResolver aResolver );
    }

    /**
     * Denotes a very simple macro resolver, which is basically a
     */
    public interface MacroResolver
    {
      // METHODS

      /**
       * Resolves the result for a given macro string, found in the given
       * "parent" element.
       * 
       * @param aMacro
       *          the macro string to resolve, cannot be <code>null</code>;
       * @param aParent
       *          the parent element containing the macro, cannot be
       *          <code>null</code>.
       * @return the result of the macro, can be <code>null</code> if no result
       *         is available.
       */
      public Object resolve( final String aMacro, final Element aParent );
    }

    // CONSTANTS

    public static final Element META = new TagElement( "meta", false );
    public static final Element H1 = new TagElement( "h1" );
    public static final Element H2 = new TagElement( "h2" );
    public static final Element H3 = new TagElement( "h3" );
    public static final Element P = new TagElement( "p" );
    public static final Element DIV = new TagElement( "div" );
    public static final Element SPAN = new TagElement( "span" );
    public static final Element TABLE = new TagElement( "table" );
    public static final Element TBODY = new TagElement( "tbody" );
    public static final Element THEAD = new TagElement( "thead" );
    public static final Element TFOOT = new TagElement( "tfoot" );
    public static final Element TR = new TagElement( "tr" );
    public static final Element TH = new TagElement( "th" );
    public static final Element TD = new TagElement( "td" );
    public static final Element BR = new TagElement( "br", false );
    public static final Element HR = new TagElement( "hr", false );

    // METHODS

    /**
     * Adds a CSS-style clause to the "general" style clause in the HTML head
     * container.
     * 
     * @param aStyleClause
     *          the CSS style clause to add, cannot be <code>null</code>.
     */
    void addCssStyle( final String aStyleClause );

    /**
     * Returns the body container of the HTML document.
     * 
     * @return the body container, never <code>null</code>.
     */
    Element getBody();

    /**
     * Returns the head container of the HTML document.
     * 
     * @return the head container, never <code>null</code>.
     */
    Element getHead();

    /**
     * Sets the title of the exported HTML document.
     * 
     * @param aTitle
     *          a title, cannot be <code>null</code>.
     */
    void setTitle( final String aTitle );

    /**
     * Returns the string representation of the HTML-structure, with all macro's
     * resolved.
     * 
     * @param aResolver
     *          the macro resolver to use, cannot be <code>null</code>.
     * @return the string representation of the HTML structure.
     */
    String toString( final MacroResolver aResolver );
  }

  /**
   * @author jawi
   */
  public interface HtmlFileExporter extends HtmlExporter
  {
    // METHODS

    /**
     * Writes the HTML export to file using the given macro resolver to resolve
     * any macros.
     * 
     * @param aResolver
     *          the macro resolver to use, cannot be <code>null</code>.
     * @throws IOException
     *           in case of I/O problems.
     */
    public void write( final MacroResolver aResolver ) throws IOException;

    /**
     * Closes this exporter and writes everything down to file.
     * 
     * @throws IOException
     *           in case of I/O problems.
     */
    void close() throws IOException;
  }

  // CONSTRUCTORS

  /**
   * Creates a new ExportUtils instance, never used.
   */
  private ExportUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Creates a CSV exporter for the given file.
   * 
   * @param aFile
   *          the file to export to, cannot be <code>null</code>.
   * @return a CSV exporter, never <code>null</code>.
   * @throws IOException
   *           in case of I/O errors.
   */
  public static CsvExporter createCsvExporter( final File aFile ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }
    return new CsvExporterImpl( aFile );
  }

  /**
   * Creates a HTML exporter.
   * 
   * @return a HTML exporter, never <code>null</code>.
   */
  public static HtmlExporter createHtmlExporter()
  {
    return new HtmlExporterImpl( true /* aIncludeDTD */);
  }

  /**
   * Creates a HTML exporter for the given file, which is written in UTF-8.
   * 
   * @param aFile
   *          the file to export to, cannot be <code>null</code>.
   * @return a HTML exporter, never <code>null</code>.
   * @throws IOException
   *           in case of I/O errors.
   */
  public static HtmlFileExporter createHtmlExporter( final File aFile ) throws IOException
  {
    if ( aFile == null )
    {
      throw new IllegalArgumentException( "File cannot be null!" );
    }
    return new HtmlFileExporterImpl( aFile );
  }
}
