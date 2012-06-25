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
   * Calculates a highlight color for the given color.
   * 
   * @param aColor
   *          the color to create a highlight color for, cannot be
   *          <code>null</code>;
   * @param aFactor
   *          the highlight factor to apply, &gt;= 0.0 && &lt;= 1.0f;
   * @return a highlighting color, never <code>null</code>.
   */
  public static Color getHighlightColor( final Color aColor, final float aFactor )
  {
    float[] hsb = Color.RGBtoHSB( aColor.getRed(), aColor.getGreen(), aColor.getBlue(), null );

    hsb[2] = Math.min( 1.0f, hsb[2] * aFactor );

    return new Color( Color.HSBtoRGB( hsb[0], hsb[1], hsb[2] ) );
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
    return ( ( 0.299 * rgb[0] ) + ( 0.587 * rgb[1] ) + ( 0.114 * rgb[2] ) );
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
    return ( ( 0.2126 * rgb[0] ) + ( 0.7152 * rgb[1] ) + ( 0.0722 * rgb[2] ) );
  }

  /**
   * Interpolates a gray-scale color between two given colors.
   * 
   * @param aBaseColor
   * @param aSecondaryColor
   * @param aDelta
   * @return
   */
  public static Color interpolate( final Color aBaseColor, final Color aSecondaryColor, final float aDelta )
  {
    float[] acomp = aSecondaryColor.getRGBComponents( null );
    float[] bcomp = aBaseColor.getRGBComponents( null );
    float[] ccomp = new float[4];

    for ( int i = 0; i < 4; i++ )
    {
      ccomp[i] = acomp[i] + ( ( bcomp[i] - acomp[i] ) * aDelta );
    }

    return new Color( ccomp[0], ccomp[1], ccomp[2], ccomp[3] );
  }

  /**
   * Parses the given color-string into a valid Color instance.
   * <p>
   * A color-string has the following form: <tt>[#]rrggbb</tt> where <tt>rr</tt>, <tt>gg</tt> and <tt>bb</tt> are the hexadecimal color values for red,
   * green and blue. The string may optionally start with a hashpound sign.
   * </p>
   * 
   * @param aColor
   *          the color string to parse as color, cannot be <code>null</code>.
   * @return the Color-instance matching the given color, never
   *         <code>null</code>.
   */
  public static final Color parseColor( final String aColor )
  {
    if ( aColor == null )
    {
      throw new IllegalArgumentException( "Color cannot be null!" );
    }

    String color = aColor.trim();
    if ( color.startsWith( "#" ) )
    {
      color = color.substring( 1 );
    }

    try
    {
      final int colorValue = Integer.parseInt( color, 16 );
      return new Color( ( colorValue >> 16 ) & 0xFF, ( colorValue >> 8 ) & 0xFF, colorValue & 0xFF );
    }
    catch ( NumberFormatException exception )
    {
      throw new IllegalArgumentException( "Given string does NOT represent a valid color!" );
    }
  }

  /**
   * Returns the given color instance as a string in the form of
   * <tt>RR GG BB</tt> in which <tt>RR</tt>, <tt>GG</tt>, <tt>BB</tt> are the
   * hexadecimal representations of red, green and blue.
   * 
   * @param aColor
   *          the color to return as a string value, cannot be <code>null</code>
   *          .
   * @return the string representing the given color.
   * @see #parseColor(String)
   */
  public static String toHexString( final Color aColor )
  {
    final StringBuilder sb = new StringBuilder();
    sb.append( String.format( "%02x", Integer.valueOf( aColor.getRed() ) ) );
    sb.append( String.format( "%02x", Integer.valueOf( aColor.getGreen() ) ) );
    sb.append( String.format( "%02x", Integer.valueOf( aColor.getBlue() ) ) );
    return sb.toString();
  }
}
