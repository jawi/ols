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
package nl.lxtreme.ols.tool.base;


import java.io.*;


/**
 * Used to mark whether or not a tool (dialog) is capable of exporting the
 * results of a tool to a file.
 */
public interface ExportAware<RESULT_TYPE>
{
  // INNER TYPES

  /**
   * The formats the export functionality should at least support.
   */
  public static enum ExportFormat
  {
    CSV, HTML;
  }

  // METHODS

  /**
   * Exports the results to a file in the given format.
   * 
   * @param aOutputFile
   *          the file to write the export results to, cannot be
   *          <code>null</code>;
   * @param aFormat
   *          the format in which to write the results, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems exporting the data to file.
   */
  void exportToFile( File aOutputFile, ExportFormat aFormat ) throws IOException;

  /**
   * Returns the result of the last tool invocation.
   * 
   * @return a tool result, can be <code>null</code>.
   */
  RESULT_TYPE getLastResult();
}
