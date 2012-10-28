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
package nl.lxtreme.ols.tool.api;


import nl.lxtreme.ols.common.*;


/**
 * Interface for pluggable tools.
 * <p>
 * A tool is something that processes a {@link DataSet} and does something
 * interesting with it, for example, decoding the data as I2C.
 * </p>
 * <p>
 * Tools can be configured by means of a {@link Configuration} object, which is
 * derived from the metatype description of a tool. This means that each tool
 * should provide a metatype description of its configuration in
 * <tt>OSGI-INF/metatype</tt>. For convenience, one can describe the metatype
 * information by means of the metatype-API of Bnd.
 * </p>
 * <p>
 * All tools implementing this interface that are added to the tools class list
 * will be automatically added to the tools menu in the client.
 * </p>
 * 
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public interface Tool
{
  // METHODS

  /**
   * Returns the category for this tool.
   * <p>
   * Each tool <em>must</em> provide a category in order to be grouped
   * correctly.
   * </p>
   * 
   * @return a category, cannot be <code>null</code>.
   */
  ToolCategory getCategory();

  /**
   * Is called to get the name for the menu entry.
   * <p>
   * The name must be unique among all tools. Should end in "..." if it opens a
   * dialog window.
   * </p>
   * 
   * @return name for this tool
   */
  String getName();

  /**
   * Invokes this tool.
   * 
   * @param aContext
   *          the context in which the tool should be run, cannot be
   *          <code>null</code>;
   * @param aConfiguration
   *          the configuration of this tool, cannot be <code>null</code>.
   * @throws ToolException
   *           in case of errors during the invocation of the tool.
   */
  void invoke( ToolContext aContext, Configuration aConfiguration ) throws ToolException;

}
