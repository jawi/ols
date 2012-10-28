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


import java.io.*;

import javax.swing.filechooser.FileFilter;


/**
 * Provides a filename filter adapter that accepts a list of {@link FileFilter}s
 * to make it easy to use one API for both Swing and AWT.
 */
public class FilenameFilterAdapter implements FilenameFilter
{
  // VARIABLES

  private final FileFilter[] fileFilters;

  // CONSTRUCTORS

  /**
   * @param aFileFilters
   *          the Swing file filters to wrap.
   */
  public FilenameFilterAdapter( final FileFilter... aFileFilters )
  {
    this.fileFilters = aFileFilters;
  }

  // METHODS

  /**
   * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
   */
  @Override
  public boolean accept( final File aDir, final String aName )
  {
    final File fileName = new File( aDir, aName );
    for ( FileFilter filter : this.fileFilters )
    {
      if ( filter.accept( fileName ) )
      {
        return true;
      }
    }
    return false;
  }

}

/* EOF */
