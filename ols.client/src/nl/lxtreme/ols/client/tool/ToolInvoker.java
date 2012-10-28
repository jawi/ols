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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.tool;


import java.awt.*;

import nl.lxtreme.ols.tool.api.*;


/**
 * Wraps a {@link Tool} for invocation from the client.
 */
public interface ToolInvoker extends org.osgi.service.cm.ManagedService
{
  // METHODS

  /**
   * Configures this tool by showing a configuration dialog on screen.
   * 
   * @param aParent
   *          the parent window to use for the dialog to show;
   * @param aContext
   *          the tool context to use.
   * @return <code>true</code> if the tool was correctly configured,
   *         <code>false</code> otherwise (user pressed cancel).
   */
  boolean configure( Window aParent, ToolContext aContext );

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
   *          <code>null</code>.
   * @throws ToolException
   *           in case of errors during the invocation of the tool.
   */
  void invoke( ToolContext aContext ) throws ToolException;

}
