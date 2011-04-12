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


import java.util.logging.*;

import nl.lxtreme.ols.util.ExportUtils.HtmlExporter;


/**
 * Provides a very basic HTML exporter.
 */
public class HtmlExporterImpl implements HtmlExporter
{
  // INNER TYPES

  private static final Logger LOG = Logger.getAnonymousLogger();

  // VARIABLES

  private final Element root;
  private final Element head;
  private final Element body;
  private final boolean includeDTD;

  // CONSTRUCTORS

  /**
   * Creates a new HtmlExporter instance.
   * 
   * @param aIncludeDTD
   *          whether or not a HTML DTD clause should be prepended to the
   *          output.
   */
  public HtmlExporterImpl( final boolean aIncludeDTD )
  {
    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "Creating new HtmlExporter..." );
    }
    this.includeDTD = aIncludeDTD;

    this.root = new TagElement( "html" );
    this.head = this.root.addChild( new TagElement( "head" ) );
    this.head.addChild( "title" );
    this.body = this.root.addChild( new TagElement( "body" ) );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addCssStyle( final String aStyleClause )
  {
    HtmlExporter.Element style = getHead().getChildByName( "style" );
    if ( style == null )
    {
      style = getHead().addChild( "style" );
      style.addAttribute( "type", "text/css" );
    }

    style.addContent( aStyleClause );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlExporter.Element getBody()
  {
    return this.body;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlExporter.Element getHead()
  {
    return this.head;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setTitle( final String aTitle )
  {
    HtmlExporter.Element title = getHead().getChildByName( "title" );
    if ( title == null )
    {
      title = getHead().addChild( "title" );
    }
    title.addContent( aTitle );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return toString( new NullMacroResolver() );
  }

  /**
   * @see nl.lxtreme.ols.util.ExportUtils.HtmlExporter#toString(nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver)
   */
  @Override
  public String toString( final MacroResolver aResolver )
  {
    final StringBuilder sb = new StringBuilder();
    if ( this.includeDTD )
    {
      sb.append( "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" )
          .append( '\n' );
    }
    sb.append( this.root.toString( aResolver ) );

    if ( LOG.isLoggable( Level.FINE ) )
    {
      LOG.fine( "+++\n" + sb + "\n---\n" );
    }

    return sb.toString();
  }
}
