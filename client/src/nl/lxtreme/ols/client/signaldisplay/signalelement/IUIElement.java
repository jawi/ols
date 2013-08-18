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
package nl.lxtreme.ols.client.signaldisplay.signalelement;


import java.awt.*;


/**
 * 
 */
public interface IUIElement
{
  // METHODS

  /**
   * Returns the main/foreground color of this UI-element.
   * 
   * @return a color, never <code>null</code>.
   */
  Color getColor();

  /**
   * Returns the label of this UI-element.
   * 
   * @return a label, can be <code>null</code>.
   */
  String getLabel();
  
  /**
   * @return
   */
  ElementGroup getGroup();

  /**
   * @return the height of this UI-element on screen, in pixels.
   */
  int getHeight();

  /**
   * @return the Y-position of this UI-element on screen, >= 0, in pixels.
   */
  int getYposition();

  /**
   * Returns whether or not this UI-element is enabled.
   * 
   * @return
   */
  boolean isEnabled();

  /**
   * @param aColor
   */
  void setColor( Color aColor );

  /**
   * Sets height to the given value.
   * 
   * @param aHeight
   *          the height to set.
   */
  void setHeight( final int aHeight );

  /**
   * Returns the label of this signal element.
   * 
   * @param aLabel
   *          the label to set, may be <code>null</code>.
   */
  void setLabel( String aLabel );

}
