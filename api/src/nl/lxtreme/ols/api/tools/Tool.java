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
package nl.lxtreme.ols.api.tools;


import java.awt.*;

import nl.lxtreme.ols.api.data.annotation.AnnotationListener;


/**
 * Interface for pluggable tools.
 * <p>
 * All tools implementing this interface that are added to the tools class list
 * will be automatically added to the tools menu in the client.
 * </p>
 * 
 * @author Michael "Mr. Sump" Poppitz
 * @author J.W. Janssen
 */
public interface Tool<RESULT_TYPE>
{
  // METHODS

  /**
   * Factory method for creating a new {@link ToolTask} instance.
   * 
   * @param aContext
   *          the tool context to use within the tool taks, cannot be
   *          <code>null</code>;
   * @param aProgressListener
   *          the tool progress listener the tool can use to report its
   *          progress, cannot be <code>null</code>.
   */
  ToolTask<RESULT_TYPE> createToolTask( ToolContext aContext, ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener );

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
   * Allows this tool controller to set up the tool by means of presenting an
   * UI.
   * 
   * @param aParent
   *          the parent window that can be used to display (modal) dialogs, can
   *          be <code>null</code>;
   * @param aContext
   *          the context in which the tool should be run, cannot be
   *          <code>null</code>;
   * @return <code>true</code> if the setup is successfully completed (the user
   *         acknowledged the setup), <code>false</code> if the setup is aborted
   *         by the user.
   */
  void invoke( final Window aParent, final ToolContext aContext );

}
