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
import nl.lxtreme.ols.api.data.*;


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
public interface Tool
{
  // INNER TYPES

  /**
   * Provides a category for pluggable tools.
   */
  public static enum Category
  {
    /** Denotes a tool that decodes something. */
    DECODER,
    /** Denotes a tool that measures something. */
    MEASURE,
    /** For tools that neither decode nor measure something. */
    OTHER;
  }

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
  Category getCategory();

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
   * This method is invoked when the tool is selected from the Tools menu. It
   * should request any missing information using a dialog and perform the
   * tool's actual task.
   * <p>
   * This method is to be called from the Event Dispatch Thread! It should be
   * able to make use of normal Swing components without having to take care
   * about which Thread it is called from.
   * </p>
   * 
   * @param aOwner
   *          the "owner" window that can be used when showing dialogs, can be
   *          <code>null</code>;
   * @param aData
   *          currently displayed capture results, cannot be <code>null</code>;
   * @param aContext
   *          the tool context, can be <code>null</code> if no context is
   *          available;
   * @param aCallback
   *          the callback to report the status of the tool to, cannot be
   *          <code>null</code>.
   */
  void process( final Window aOwner, final DataContainer aData, final ToolContext aContext,
      final AnalysisCallback aCallback );
}
