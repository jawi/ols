/*
 * Copyright (c) 2007, Romain Guy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *   * Neither the name of the TimingFramework project nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package nl.lxtreme.ols.util.swing.component;


import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;


/**
 * Provides a stack layout, which provides a simple alternative to
 * {@link JLayeredPane}s.
 * 
 * @author Romain Guy <romain.guy@mac.com>
 */
public class StackLayout implements LayoutManager2
{
  // CONSTANTS

  public static final String BOTTOM = "bottom";
  public static final String TOP = "top";

  // VARIABLES

  private final List<Component> components = new LinkedList<Component>();

  // METHODS

  /**
   * @see java.awt.LayoutManager2#addLayoutComponent(java.awt.Component,
   *      java.lang.Object)
   */
  public void addLayoutComponent( final Component aComp, final Object aConstraints )
  {
    synchronized ( aComp.getTreeLock() )
    {
      if ( BOTTOM.equals( aConstraints ) )
      {
        this.components.add( 0, aComp );
      }
      else
      {
        this.components.add( aComp );
      }
    }
  }

  /**
   * @see java.awt.LayoutManager#addLayoutComponent(java.lang.String,
   *      java.awt.Component)
   */
  public void addLayoutComponent( final String aName, final Component aComponent )
  {
    addLayoutComponent( aComponent, TOP );
  }

  /**
   * @see java.awt.LayoutManager2#getLayoutAlignmentX(java.awt.Container)
   */
  public float getLayoutAlignmentX( final Container aTarget )
  {
    return 0.5f;
  }

  /**
   * @see java.awt.LayoutManager2#getLayoutAlignmentY(java.awt.Container)
   */
  public float getLayoutAlignmentY( final Container aTarget )
  {
    return 0.5f;
  }

  /**
   * @see java.awt.LayoutManager2#invalidateLayout(java.awt.Container)
   */
  public void invalidateLayout( final Container aTarget )
  {
    // NO-op
  }

  /**
   * @see java.awt.LayoutManager#layoutContainer(java.awt.Container)
   */
  public void layoutContainer( final Container aParent )
  {
    synchronized ( aParent.getTreeLock() )
    {
      int width = aParent.getWidth();
      int height = aParent.getHeight();

      Rectangle bounds = new Rectangle( 0, 0, width, height );

      int componentsCount = this.components.size();

      for ( int i = 0; i < componentsCount; i++ )
      {
        Component comp = this.components.get( i );
        comp.setBounds( bounds );
        aParent.setComponentZOrder( comp, componentsCount - i - 1 );
      }
    }
  }

  /**
   * @see java.awt.LayoutManager2#maximumLayoutSize(java.awt.Container)
   */
  public Dimension maximumLayoutSize( final Container aTarget )
  {
    return new Dimension( Integer.MAX_VALUE, Integer.MAX_VALUE );
  }

  /**
   * @see java.awt.LayoutManager#minimumLayoutSize(java.awt.Container)
   */
  public Dimension minimumLayoutSize( final Container aParent )
  {
    synchronized ( aParent.getTreeLock() )
    {
      int width = 0;
      int height = 0;

      for ( Component comp : this.components )
      {
        Dimension size = comp.getMinimumSize();
        width = Math.max( size.width, width );
        height = Math.max( size.height, height );
      }

      Insets insets = aParent.getInsets();
      width += insets.left + insets.right;
      height += insets.top + insets.bottom;

      return new Dimension( width, height );
    }
  }

  /**
   * @see java.awt.LayoutManager#preferredLayoutSize(java.awt.Container)
   */
  public Dimension preferredLayoutSize( final Container aParent )
  {
    synchronized ( aParent.getTreeLock() )
    {
      int width = 0;
      int height = 0;

      for ( Component comp : this.components )
      {
        Dimension size = comp.getPreferredSize();
        width = Math.max( size.width, width );
        height = Math.max( size.height, height );
      }

      Insets insets = aParent.getInsets();
      width += insets.left + insets.right;
      height += insets.top + insets.bottom;

      return new Dimension( width, height );
    }
  }

  /**
   * @see java.awt.LayoutManager#removeLayoutComponent(java.awt.Component)
   */
  public void removeLayoutComponent( final Component aComponent )
  {
    synchronized ( aComponent.getTreeLock() )
    {
      this.components.remove( aComponent );
    }
  }
}
