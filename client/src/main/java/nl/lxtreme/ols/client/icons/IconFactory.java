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
package nl.lxtreme.ols.client.icons;


import java.awt.*;
import java.net.*;
import java.util.logging.*;

import javax.swing.*;

import org.osgi.framework.*;

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

  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( IconFactory.class.getName() );

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
    try
    {
      final URL url = getResource( aIconName );
      if ( url != null )
      {
        return new ImageIcon( url );
      }
    }
    catch ( Exception exception )
    {
      exception.printStackTrace();
    }
    return new ImageIcon();
  }

  /**
   * Creates an image for the resource with the given name.
   *
   * @param aIconName
   *          the (symbolic) name of the resource to load as image, cannot be
   *          <code>null</code>.
   * @return the image for the given resource name, never <code>null</code>.
   */
  public static Image createImage( final String aImageName )
  {
    final URL url = getResource( aImageName );
    return Toolkit.getDefaultToolkit().createImage( url );
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
    return new CompoundTextIcon( aIconName, aText, SwingConstants.EAST );
  }

  private static URL getResource( final String aIconName )
  {
    URL resource;
    Bundle bundle = FrameworkUtil.getBundle( IconLocator.class );
    if ( bundle == null )
    {
      // Old-fashioned way...
      resource = IconLocator.class.getResource( aIconName );
      LOG.log( Level.FINE, "Get icon resource without bundle: {0} => {1}...", new Object[] { aIconName, resource } );
    }
    else
    {
      String path = IconLocator.class.getPackage().getName().replaceAll( "\\.", "/" );

      resource = bundle.getResource( path + "/" + aIconName );
      LOG.log( Level.FINE, "Get icon resource from bundle: {0} => {1}...", new Object[] { aIconName, resource } );
    }
    return resource;
  }
}
