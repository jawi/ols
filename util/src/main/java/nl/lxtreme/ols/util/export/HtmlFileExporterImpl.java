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
package nl.lxtreme.ols.util.export;


import java.io.*;

import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter;


/**
 * Provides a file-writing HTML exporter.
 */
public class HtmlFileExporterImpl extends HtmlExporterImpl implements HtmlFileExporter
{
  // VARIABLES

  public final Writer writer;

  // CONSTRUCTORS

  /**
   * Creates a new HtmlFileExporterImpl instance for writing to the given file
   * in UTF-8.
   * 
   * @param aFile
   *          the file to write to, cannot be <code>null</code>;
   * @throws IOException
   *           in case of I/O problems.
   */
  public HtmlFileExporterImpl( final File aFile ) throws IOException
  {
    this( new OutputStreamWriter( new FileOutputStream( aFile ), "UTF8" ) );
  }

  /**
   * Creates a new HtmlFileExporterImpl instance for writing to the given
   * Writer.
   * 
   * @param aWriter
   *          the writer to write to, cannot be <code>null</code>;
   */
  protected HtmlFileExporterImpl( final Writer aWriter )
  {
    super( true /* aIncludeDTD */);
    // Ensure for HTML files the content type is set...
    getHead().addChild( HtmlExporter.META ).addAttribute( "http-equiv", "Content-type" )
        .addAttribute( "content", "text/html;charset=UTF-8" );

    this.writer = aWriter;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    try
    {
      this.writer.flush();
    }
    finally
    {
      this.writer.close();
    }
  }

  /**
   * @see nl.lxtreme.ols.util.ExportUtils.HtmlFileExporter#write(nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver)
   */
  @Override
  public void write( final MacroResolver aResolver ) throws IOException
  {
    this.writer.write( toString( aResolver ) );
  }
}
