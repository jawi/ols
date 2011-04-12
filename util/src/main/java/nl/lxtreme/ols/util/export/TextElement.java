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


import java.util.*;
import java.util.regex.*;

import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Attribute;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;


/**
 * Provides a simple text-element.
 */
public final class TextElement implements Element
{
  // CONSTANTS

  static final Pattern MACRO_PATTERN = Pattern.compile( ".*\\{([\\w-]+)\\}.*" );

  // VARIABLES

  private String value;
  private Element parent;

  // CONSTRUCTORS

  /**
   * Creates a new {@link TextElement} instance.
   * 
   * @param aValue
   *          the value of this text element, may be <code>null</code>.
   */
  public TextElement( final String aValue )
  {
    this.value = aValue == null ? "" : aValue;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addAttribute( final String aName, final String aValue )
  {
    throw new IllegalStateException( "Cannot add attributes to text!" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addChild( final Element aChild )
  {
    throw new IllegalStateException( "Cannot add attributes to text!" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addChild( final String aName )
  {
    throw new IllegalStateException( "Cannot add attributes to text!" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addContent( final String... aValue )
  {
    throw new IllegalStateException( "Cannot add attributes to text!" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element clone()
  {
    try
    {
      final TextElement result = ( TextElement )super.clone();
      result.value = this.value;
      return result;
    }
    catch ( CloneNotSupportedException exception )
    {
      throw new RuntimeException( exception );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<Attribute> getAttributes()
  {
    return Collections.emptyList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element getChildByName( final String aName )
  {
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<Element> getChildren()
  {
    return Collections.emptyList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return null;
  }

  /**
   * @param aParent
   *          the parent to set
   */
  public void setParent( final Element aParent )
  {
    this.parent = aParent;
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
   * @see nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element#toString(nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver)
   */
  @Override
  public String toString( final MacroResolver aResolver )
  {
    final Matcher matcher = MACRO_PATTERN.matcher( this.value );
    if ( matcher.matches() )
    {
      Object result = aResolver.resolve( matcher.group( 1 ), this.parent );
      if ( result != null )
      {
        return String.valueOf( result );
      }
      else
      {
        return "";
      }
    }
    return this.value;
  }
}
