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
package nl.lxtreme.ols.client.annotation.export.impl;


import java.io.*;
import java.util.*;

import nl.lxtreme.ols.client.annotation.export.ExportUtils.CsvBuilder;
import nl.lxtreme.ols.client.annotation.export.ExportUtils.CsvRowBuilder;
import nl.lxtreme.ols.client.annotation.export.ExportUtils.DataConverter;


/**
 * 
 */
public class CsvStreamBuilder implements CsvBuilder
{
  // INNER TYPES

  /**
   * Implementation of {@link CsvRowBuilder} that directly writes added values
   * to an underlying stream.
   */
  private class CsvRowStreamBuilder implements CsvRowBuilder
  {
    private boolean firstColumn = true;

    /**
     * {@inheritDoc}
     */
    @Override
    public CsvRowBuilder add( final Object aValue ) throws IOException
    {
      if ( !this.firstColumn )
      {
        writeSeparator();
      }
      writeQuotedValue( aValue );
      this.firstColumn = false;
      return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public CsvBuilder endRow() throws IOException
    {
      writeLineTerminator();
      return CsvStreamBuilder.this;
    }
  }

  // VARIABLES

  private final boolean windows;
  private final Set<DataConverter> converters;
  private final BufferedWriter writer;

  // CONSTRUCTORS

  /**
   * Creates a new {@link CsvStreamBuilder} instance.
   * 
   * @param file
   * @throws FileNotFoundException
   *           in case the given file was not found.
   */
  public CsvStreamBuilder( final File aFile ) throws FileNotFoundException
  {
    this( new FileOutputStream( aFile ) );
  }

  /**
   * Creates a new {@link CsvStreamBuilder} instance.
   * 
   * @param aOutputStream
   *          the output stream to stream the CSV results to.
   */
  public CsvStreamBuilder( final OutputStream aOutputStream )
  {
    this.writer = new BufferedWriter( new OutputStreamWriter( aOutputStream ) );
    this.converters = new HashSet<DataConverter>();

    String osName = System.getProperty( "os.name" ).toLowerCase();
    this.windows = osName.indexOf( "win" ) >= 0;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public CsvBuilder addDataConverter( final DataConverter aConverter )
  {
    this.converters.add( aConverter );
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CsvRowBuilder addRow()
  {
    return new CsvRowStreamBuilder();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    try
    {
      this.writer.close();
    }
    catch ( IOException ignored )
    {
      // ignore...
    }
  }

  /**
   * @throws IOException
   */
  final void writeLineTerminator() throws IOException
  {
    this.writer.newLine();
  }

  /**
   * @param aValue
   * @throws IOException
   */
  final void writeQuotedValue( final Object aValue ) throws IOException
  {
    Object value = aValue;
    for ( DataConverter converter : this.converters )
    {
      if ( converter.canHandle( aValue ) )
      {
        value = converter.getAsString( aValue );
      }
    }
    this.writer.write( quote( value ) );
  }

  /**
   * @throws IOException
   */
  final void writeSeparator() throws IOException
  {
    if ( this.windows )
    {
      this.writer.write( ';' );
    }
    else
    {
      this.writer.write( ',' );
    }
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
