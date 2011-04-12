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
import java.util.logging.*;

import nl.lxtreme.ols.util.ExportUtils.CsvExporter;


/**
 * Provides a CSV exporter implementation.
 */
public class CsvExporterImpl implements CsvExporter
{
  // CONSTANTS

  private static final Logger LOG = Logger.getAnonymousLogger();

  // VARIABLES

  private final char delimiter;

  private BufferedWriter writer;
  private int headerCount = -1;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CsvExporterImpl} instance.
   * 
   * @param aFile
   *          the file to export to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O exceptions.
   */
  public CsvExporterImpl( final File aFile ) throws IOException
  {
    this( aFile, ',' );
  }

  /**
   * Creates a new {@link CsvExporterImpl} instance.
   * 
   * @param aFile
   *          the file to export to, cannot be <code>null</code>;
   * @param aDelimiter
   *          the CSV delimiter to use.
   * @throws IOException
   *           in case of I/O exceptions.
   */
  public CsvExporterImpl( final File aFile, final char aDelimiter ) throws IOException
  {
    this.writer = new BufferedWriter( new FileWriter( aFile ) );
    this.delimiter = aDelimiter;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addRow( final Object... aValues ) throws IOException
  {
    if ( aValues.length != this.headerCount )
    {
      LOG.warning( "Number of cells not equal to header count! Header count = " + this.headerCount + ", cell count = "
          + aValues.length );
    }

    // If set, use the header count as leading cell count...
    final int length = Math.max( Math.min( this.headerCount, aValues.length ), this.headerCount );

    for ( int i = 0; i < length; i++ )
    {
      final Object value = i < aValues.length ? aValues[i] : null;

      this.writer.append( quote( value ) );

      if ( i < length - 1 )
      {
        this.writer.append( this.delimiter );
      }
    }
    this.writer.newLine();
  }

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
      this.writer = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setHeaders( final String... aHeaders ) throws IOException
  {
    this.headerCount = aHeaders.length;
    for ( int i = 0; i < aHeaders.length; i++ )
    {
      this.writer.append( quote( aHeaders[i] ) );
      if ( i < aHeaders.length - 1 )
      {
        this.writer.append( this.delimiter );
      }
    }
    this.writer.newLine();
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
