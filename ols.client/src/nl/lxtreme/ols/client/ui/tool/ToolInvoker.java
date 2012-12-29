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
package nl.lxtreme.ols.client.ui.tool;


import java.awt.*;

import org.osgi.service.cm.*;

import nl.lxtreme.ols.tool.api.*;


/**
 * Wraps a {@link Tool} for invocation from the client.
 */
public interface ToolInvoker extends org.osgi.service.cm.ManagedService
{
  // CONSTANTS

  /** The topic prefix */
  String TOPIC_PREFIX = ToolInvoker.class.getPackage().getName().replaceAll( "\\.", "/" );

  /** All of the topics used by this event producer. */
  String TOPIC_ANY = TOPIC_PREFIX.concat( "/*" );

  /**
   * Used to report the progress of the running tool. The event provides
   * information about the tool itself, the date on which it was started and the
   * progress of the tool.
   */
  String TOPIC_TOOL_PROGRESS = TOPIC_PREFIX.concat( "/progress" );
  /**
   * Used to report the status of the running tool. The event provides
   * information about the tool itself, the date on which it was started and the
   * state of the tool.
   */
  String TOPIC_TOOL_STATUS = TOPIC_PREFIX.concat( "/status" );

  String KEY_TOOL_NAME = "toolName";
  String KEY_TOOL_START_TIME = "startTime";
  String KEY_TOOL_PROGRESS = "progress";
  String KEY_TOOL_STATE = "state";
  String KEY_TOOL_EXCEPTION = "exception";

  String TOOL_STATUS_STARTED = "started";
  String TOOL_STATUS_CANCELLED = "cancelled";
  String TOOL_STATUS_FAILED = "failed";
  String TOOL_STATUS_SUCCESS = "success";

  // METHODS

  /**
   * Configures this tool by showing a configuration dialog on screen.
   * 
   * @param aParent
   *          the parent window to use for the dialog to show;
   * @param aListener
   *          the callback to invoke when the configuration is changed.
   * @return <code>true</code> if the tool was correctly configured,
   *         <code>false</code> otherwise (user pressed cancel).
   */
  void configure( Window aParent, ConfigurationListener aListener );

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
  void invoke() throws ToolException;

}
