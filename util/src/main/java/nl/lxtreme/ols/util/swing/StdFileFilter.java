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


import javax.swing.filechooser.*;


/**
 * Provides a set of standard file filters.
 */
public final class StdFileFilter
{
  // CONSTANTS

  public static final FileFilter HTML = new FileNameExtensionFilter( "HTML files", "htm", "html" );
  public static final FileFilter CSV = new FileNameExtensionFilter( "CSV files", "csv" );
  public static final FileFilter XML = new FileNameExtensionFilter( "XML files", "xml" );

  // CONSTRUCTORS

  /**
   * 
   */
  private StdFileFilter()
  {
    // NO-op
  }
}

/* EOF */
