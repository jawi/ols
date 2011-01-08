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
package nl.lxtreme.ols.client.icons;


import java.net.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.component.icon.*;


/**
 * Provides some convenience methods for handling icons.
 */
public final class IconFactory
{
  // INNER TYPES

  /**
   * Denotes a compound icon consisting of an icon with some text overlay.
   */
  public static class CompoundTextIcon extends TextOverlayIcon
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // CONSTRUCTORS

    /**
     * Creates a new CompoundTextIcon instance, placing the text in the center.
     * 
     * @param aIconName
     *          the (symbolic) name of the icon;
     * @param aTextOverlay
     *          the text that is to overlayed;
     */
    public CompoundTextIcon( final String aIconName, final String aTextOverlay )
    {
      super( createIcon( aIconName ), aTextOverlay );
    }

    /**
     * Creates a new CompoundTextIcon instance, placing the text at the given
     * position.
     * 
     * @param aIconName
     *          the (symbolic) name of the icon;
     * @param aTextOverlay
     *          the text that is to overlayed;
     * @param aPosition
     *          on of the {@link SwingConstants} values that denotes the overlay
     *          position. E.g.: {@link SwingConstants#NORTH_EAST}.
     */
    public CompoundTextIcon( final String aIconName, final String aTextOverlay, final int aPosition )
    {
      super( createIcon( aIconName ), aTextOverlay, aPosition );
    }
  }

  // CONSTRUCTORS

  /**
   * Creates a new IconFactory, never used.
   */
  private IconFactory()
  {
    super();
  }

  // METHODS

  /**
   * Creates an icon for the resource with the given name.
   * 
   * @param aIconName
   *          the (symbolic) name of the resource to load as icon, cannot be
   *          <code>null</code>.
   * @return the icon for the given resource name, never <code>null</code>.
   */
  public static Icon createIcon( final String aIconName )
  {
    final URL url = IconLocator.class.getResource( aIconName );
    return new ImageIcon( url );
  }

  /**
   * Creates an icon with a text overlay positioned at the south east corner of
   * the icon.
   * 
   * @param aIconName
   *          the (symbolic) name of the resource to load as icon, cannot be
   *          <code>null</code>;
   * @param aText
   *          the text to overlay, cannot be <code>null</code>.
   * @return the icon for the given resource name, never <code>null</code>.
   */
  public static Icon createOverlayIcon( final String aIconName, final String aText )
  {
    return new CompoundTextIcon( aIconName, aText, SwingConstants.SOUTH_EAST );
  }
}
