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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.colorscheme;


import java.util.*;


/**
 * Provides access to all current color schemes.
 */
public interface ColorSchemeProvider
{
  // METHODS

  /**
   * Retrieves the color scheme by its name.
   * 
   * @param aName
   *          the name of the color scheme, can be <code>null</code> or empty.
   * @return a {@link Properties} map with the requested color scheme, or
   *         <code>null</code> if no such scheme exists.
   */
  Properties getColorScheme( String aName );

  /**
   * @return an array with color scheme names, never <code>null</code>.
   */
  String[] getColorSchemeNames();

}
