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

import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Attribute;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;


/**
 * Provides a "tag" element, such as &lt;div&gt;-elements.
 */
public final class TagElement implements Element
{
  // VARIABLES

  private String name;
  private List<Attribute> attributes;
  private List<Element> children;
  private boolean needsCloseTag;

  // CONSTRUCTORS

  /**
   * Creates a new {@link TagElement} instance.
   * 
   * @param aName
   *          the name of this tag element, cannot be <code>null</code>.
   */
  public TagElement( final String aName )
  {
    this( aName, true /* aNeedsCloseTag */);
  }

  /**
   * Creates a new {@link TagElement} instance.
   * 
   * @param aName
   *          the name of this tag element, cannot be <code>null</code>;
   * @param aNeedsCloseTag
   *          <code>true</code> if a separate closing tag is needed,
   *          <code>false</code> if this tag can be closed in an inline manner.
   */
  public TagElement( final String aName, final boolean aNeedsCloseTag )
  {
    this.name = aName;
    this.needsCloseTag = aNeedsCloseTag;
    this.attributes = new ArrayList<Attribute>();
    this.children = new ArrayList<Element>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addAttribute( final String aName, final String aValue )
  {
    final Attribute attr = new AttributeImpl( aName, aValue );
    this.attributes.add( attr );
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addChild( final Element aChild )
  {
    if ( !this.needsCloseTag )
    {
      throw new IllegalStateException( "Cannot add children to tag that doesn't have close tag!" );
    }
    final Element child = aChild.clone();
    this.children.add( child );
    return child;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addChild( final String aName )
  {
    if ( !this.needsCloseTag )
    {
      throw new IllegalStateException( "Cannot add children to tag that doesn't have close tag!" );
    }
    final Element child = new TagElement( aName );
    this.children.add( child );
    return child;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element addContent( final String... aValue )
  {
    if ( !this.needsCloseTag )
    {
      throw new IllegalStateException( "Cannot set contents for tag that doesn't have close tag!" );
    }
    for ( String value : aValue )
    {
      final TextElement element = new TextElement( value );
      element.setParent( this );
      this.children.add( element );
    }
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element clone()
  {
    try
    {
      final TagElement clone = ( TagElement )super.clone();
      clone.name = this.name;
      clone.needsCloseTag = this.needsCloseTag;
      clone.attributes = new ArrayList<Attribute>( this.attributes.size() );
      for ( Attribute attr : getAttributes() )
      {
        clone.attributes.add( attr.clone() );
      }
      clone.children = new ArrayList<Element>( this.children.size() );
      for ( Element child : getChildren() )
      {
        final Element childClone = child.clone();
        if ( childClone instanceof TextElement )
        {
          ( ( TextElement )childClone ).setParent( clone );
        }
        clone.children.add( childClone );
      }
      return clone;
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
    return this.attributes;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element getChildByName( final String aName )
  {
    for ( Element child : this.children )
    {
      if ( aName.equalsIgnoreCase( child.getName() ) )
      {
        return child;
      }
    }
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<Element> getChildren()
  {
    return this.children;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return this.name;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    return toString( new NullMacroResolver() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString( final MacroResolver aResolver )
  {
    final StringBuilder sb = new StringBuilder();
    sb.append( '<' ).append( this.name );

    if ( !this.attributes.isEmpty() )
    {
      for ( int i = 0; i < this.attributes.size(); i++ )
      {
        final Attribute attribute = this.attributes.get( i );

        sb.append( ' ' );
        sb.append( attribute.toString( aResolver ) );
      }
    }

    sb.append( '>' );

    if ( this.needsCloseTag )
    {
      for ( int i = 0; i < this.children.size(); i++ )
      {
        final Element child = this.children.get( i );

        sb.append( child.toString( aResolver ) );
      }
      sb.append( "</" ).append( this.name ).append( '>' );
    }

    return sb.toString();
  }
}
