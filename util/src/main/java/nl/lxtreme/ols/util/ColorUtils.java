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


import java.awt.*;


/**
 * Provides some utilities for fiddling with colors.
 */
public final class ColorUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new ColorUtils instance. Never used.
   */
  private ColorUtils()
  {
    super();
  }

  // METHODS

  /**
   * Creates a contrasting color, based on the "perceived luminance" of the
   * given color.
   * <p>
   * See also:
   * <tt>http://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color</tt>
   * , and
   * <tt>http://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color</tt>
   * .
   * </p>
   * 
   * @param aColor
   *          the color to create a contrasting color for, cannot be
   *          <code>null</code>.
   * @return a contrasting color, never <code>null</code>.
   */
  public static Color getContrastColor( final Color aColor )
  {
    // Counting the perceptive luminance - human eye favors green color...
    double pl = 1.0 - getPerceivedLuminance( aColor );
    if ( pl < 0.5 )
    {
      // bright colors -> use black
      return Color.BLACK;
    }

    // dark colors -> use white
    return Color.WHITE;
  }

  /**
   * Returns the Digital CCIR601 luminance value of the given color.
   * 
   * @param aColor
   *          the color to return the luminance value for, cannot be
   *          <code>null</code>.
   * @return a luminance value, 0.0..1.0.
   */
  public static double getPerceivedLuminance( final Color aColor )
  {
    final float[] rgb = aColor.getRGBComponents( null );
    // (0.299*R + 0.587*G + 0.114*B)
    return ( 0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2] );
  }

  /**
   * Returns the Photometric/digital ITU-R luminance value of the given color.
   * 
   * @param aColor
   *          the color to return the luminance value for, cannot be
   *          <code>null</code>.
   * @return a luminance value, 0.0..1.0.
   */
  public static double getStandardLuminance( final Color aColor )
  {
    final float[] rgb = aColor.getRGBComponents( null );
    // (0.2126*R) + (0.7152*G) + (0.0722*B)
    return ( 0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2] );
  }
}
