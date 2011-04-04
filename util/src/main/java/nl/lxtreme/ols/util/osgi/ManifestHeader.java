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
package nl.lxtreme.ols.util.osgi;


import java.io.*;
import java.util.*;


/**
 * Denotes a manifest header entry, containing of a key and a value.
 */
public final class ManifestHeader implements Serializable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final String key;
  private final String value;

  // CONSTRUCTORS

  /**
   * Creates a new ManifestHeader instance.
   * 
   * @param aKey
   *          the key of the header;
   * @param aValue
   *          the value of the header.
   */
  public ManifestHeader( final String aKey, final String aValue )
  {
    this.key = aKey;
    this.value = ( aValue == null ) ? "" : aValue.trim();
  }

  // METHODS

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
    if ( ( aObject == null ) || !( aObject instanceof ManifestHeader ) )
    {
      return false;
    }

    final ManifestHeader other = ( ManifestHeader )aObject;
    if ( this.key == null )
    {
      if ( other.key != null )
      {
        return false;
      }
    }
    else if ( !this.key.equals( other.key ) )
    {
      return false;
    }

    if ( this.value == null )
    {
      if ( other.value != null )
      {
        return false;
      }
    }
    else if ( !this.value.equals( other.value ) )
    {
      return false;
    }

    return true;
  }

  /**
   * Returns the key-name of this header.
   * 
   * @return the key name, as string, never <code>null</code>.
   */
  public String getKey()
  {
    return this.key;
  }

  /**
   * Returns the value of this header.
   * 
   * @return the value, as string, never <code>null</code>.
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
    result = prime * result + ( ( this.key == null ) ? 0 : this.key.hashCode() );
    result = prime * result + ( ( this.value == null ) ? 0 : this.value.hashCode() );
    return result;
  }

  /**
   * Splits this header's value on ", "-tokens.
   * 
   * @return the splitted value, never <code>null</code>.
   */
  public String[] splitValue()
  {
    final StringTokenizer tokenizer = new StringTokenizer( getValue(), ", " );

    final int count = tokenizer.countTokens();

    final String[] result = new String[count];
    for ( int i = 0; tokenizer.hasMoreTokens() && ( i < count ); i++ )
    {
      result[i] = tokenizer.nextToken();
    }

    return result;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return getKey().concat( ": " ).concat( getValue() );
  }
}
