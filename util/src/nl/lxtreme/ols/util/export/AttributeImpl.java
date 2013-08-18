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


import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Attribute;
import nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver;


/**
 * Provides a default implementation of {@link Attribute}.
 */
public class AttributeImpl implements Attribute
{
  // VARIABLES

  private String name;
  private String value;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AttributeImpl} instance.
   * 
   * @param aName
   *          the name of this attribute, which will also be its value, cannot
   *          be <code>null</code>.
   */
  public AttributeImpl( final String aName )
  {
    this( aName, aName );
  }

  /**
   * Creates a new {@link AttributeImpl} instance.
   * 
   * @param aName
   *          the name of this attribute, cannot be <code>null</code>;
   * @param aValue
   *          the value of this attribute, cannot be <code>null</code>.
   */
  public AttributeImpl( final String aName, final String aValue )
  {
    this.name = aName;
    this.value = aValue;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Attribute clone()
  {
    try
    {
      final AttributeImpl result = ( AttributeImpl )super.clone();
      result.name = this.name;
      result.value = this.value;
      return result;
    }
    catch ( CloneNotSupportedException exception )
    {
      throw new RuntimeException( exception );
    }
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof Attribute ) )
    {
      return false;
    }

    Attribute other = ( Attribute )aObject;
    if ( !this.name.equals( other.getName() ) )
    {
      return false;
    }

    if ( !this.value.equals( other.getValue() ) )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the name of this attribute.
   * 
   * @return the name of this attribute, never <code>null</code>.
   */
  public String getName()
  {
    return this.name;
  }

  /**
   * Returns the value of this attribute.
   * 
   * @return the value of this attribute, never <code>null</code>.
   */
  public String getValue()
  {
    return this.value;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( this.name == null ) ? 0 : this.name.hashCode() );
    result = prime * result + ( ( this.value == null ) ? 0 : this.value.hashCode() );
    return result;
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
   * @see nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Attribute#toString(nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver)
   */
  @Override
  public String toString( final MacroResolver aResolver )
  {
    return this.name + "='" + this.value + "'";
  }
}
