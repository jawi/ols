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
package nl.lxtreme.ols.api.data.export;


import java.io.*;

import nl.lxtreme.ols.api.data.*;


/**
 * Provides an exporter for exporting data to an external entity, such as a
 * file.
 */
public interface Exporter
{
  // METHODS

  /**
   * Exports the given data container to the given writer.
   * 
   * @param aContainer
   *          the data container to export, can never be <code>null</code>;
   * @param aWriter
   *          the writer to write the export to, can never be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  void export( final DataContainer aContainer, final Writer aWriter ) throws IOException;

  /**
   * Returns the file extensions supported by this exporter.
   * <p>
   * For example, when exporting to an image, this method can yield {"gif",
   * "png"} to denote that it supports GIF and PNG files. This knowledge can be
   * used to let the user specify a file name with the correct extension.
   * </p>
   * 
   * @return the supported file extensions, never <code>null</code>, but may be
   *         empty.
   */
  String[] getFilenameExtentions();

  /**
   * Returns the name of this exporter.
   * 
   * @return a name, never <code>null</code> or empty.
   */
  String getName();
}
