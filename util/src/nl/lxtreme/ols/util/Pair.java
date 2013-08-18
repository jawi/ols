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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.util;


/**
 * Denotes a pair of things with possible different types.
 */
public final class Pair<T, V>
{
  // VARIABLES

  private final T left;
  private final V right;

  // CONSTRUCTORS

  /**
   * Creates a new Pair instance.
   * 
   * @param aLeft
   *          the left-hand side of this pair;
   * @param aRight
   *          the right-hand side of this pair.
   */
  public Pair( final T aLeft, final V aRight )
  {
    this.left = aLeft;
    this.right = aRight;
  }

  // METHODS

  /**
   * Factory method to create a new pair of two given values.
   * 
   * @param aLeft
   *          the left-hand side of the pair;
   * @param aRight
   *          the right-hand side of the pair.
   * @return a new pair instance, never <code>null</code>.
   */
  public static <T, V> Pair<T, V> of( final T aLeft, final V aRight )
  {
    return new Pair<T, V>( aLeft, aRight );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals( final Object aObject )
  {
    if ( this == aObject )
    {
      return true;
    }
    if ( ( aObject == null ) || !( aObject instanceof Pair ) )
    {
      return false;
    }

    final Pair<?, ?> other = ( Pair<?, ?> )aObject;
    if ( this.left == null )
    {
      if ( other.left != null )
      {
        return false;
      }
    }
    else if ( !this.left.equals( other.left ) )
    {
      return false;
    }

    if ( this.right == null )
    {
      if ( other.right != null )
      {
        return false;
      }
    }
    else if ( !this.right.equals( other.right ) )
    {
      return false;
    }

    return true;
  }

  /**
   * @return the left-hand side, can be <code>null</code>.
   */
  public T getLeft()
  {
    return this.left;
  }

  /**
   * @return the right-hand side, can be <code>null</code>.
   */
  public V getRight()
  {
    return this.right;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = ( prime * result ) + ( ( this.left == null ) ? 0 : this.left.hashCode() );
    result = ( prime * result ) + ( ( this.right == null ) ? 0 : this.right.hashCode() );
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString()
  {
    StringBuilder builder = new StringBuilder();
    builder.append( "Pair<" );
    builder.append( this.left );
    builder.append( ", " );
    builder.append( this.right );
    builder.append( ">" );
    return builder.toString();
  }

}
