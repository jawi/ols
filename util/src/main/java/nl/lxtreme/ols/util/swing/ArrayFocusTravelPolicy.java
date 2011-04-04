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
package nl.lxtreme.ols.util.swing;


import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.util.*;


/**
 * A FocusTraversalPolicy based on the order of the elements of an array.
 * 
 * @author stefant
 */
public class ArrayFocusTravelPolicy extends FocusTraversalPolicy
{
  // VARIABLES

  private final Component components[];

  // CONSTRUCTORS

  /**
   * Creates a new {@link ArrayFocusTravelPolicy} instance.
   * 
   * @param aComponents
   *          the (ordered!) component to traverse the focus for.
   */
  public ArrayFocusTravelPolicy( final Component... aComponents )
  {
    this.components = aComponents;
  }

  /**
   * Creates a new {@link ArrayFocusTravelPolicy} instance.
   * 
   * @param aComponents
   *          the (ordered!) component to traverse the focus for.
   */
  public ArrayFocusTravelPolicy( final List<Component> aComponents )
  {
    this.components = aComponents.toArray( new Component[aComponents.size()] );
  }

  // METHODS

  /**
   * @see java.awt.FocusTraversalPolicy#getComponentAfter(java.awt.Container,
   *      java.awt.Component)
   */
  @Override
  public Component getComponentAfter( final Container container, final Component component )
  {
    return cycle( component, 1 );
  }

  /**
   * @see java.awt.FocusTraversalPolicy#getComponentBefore(java.awt.Container,
   *      java.awt.Component)
   */
  @Override
  public Component getComponentBefore( final Container container, final Component component )
  {
    return cycle( component, -1 );
  }

  /**
   * @see java.awt.FocusTraversalPolicy#getDefaultComponent(java.awt.Container)
   */
  @Override
  public Component getDefaultComponent( final Container container )
  {
    return getFirstComponent( container );
  }

  /**
   * @see java.awt.FocusTraversalPolicy#getFirstComponent(java.awt.Container)
   */
  @Override
  public Component getFirstComponent( final Container container )
  {
    return this.components[0];
  }

  /**
   * @see java.awt.FocusTraversalPolicy#getLastComponent(java.awt.Container)
   */
  @Override
  public Component getLastComponent( final Container container )
  {
    return this.components[this.components.length - 1];
  }

  /**
   * @param aLastComponent
   * @param aDelta
   * @return
   */
  private Component cycle( final Component aLastComponent, final int aDelta )
  {
    int size = this.components.length;
    int index = findIndexFromComponent( aLastComponent );
    if ( index < 0 )
    {
      return null;
    }

    // note that size is added before due to java's % semantic!
    int newIndex = ( size + index + aDelta ) % size;
    return this.components[newIndex];
  }

  /**
   * @param aComponent
   * @return
   */
  private int findIndexFromComponent( final Component aComponent )
  {
    final int size = this.components.length;
    for ( int i = 0; i < size; i++ )
    {
      final Component currentC = this.components[i];
      if ( currentC == aComponent )
      {
        return i;
      }
    }
    return -1;
  }
}
