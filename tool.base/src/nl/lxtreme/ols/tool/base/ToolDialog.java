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
package nl.lxtreme.ols.tool.base;


/**
 * Provides a common interface for all tool dialogs.
 */
public interface ToolDialog
{
  // METHODS

  /**
   * Called to cancel the tool (if it is running).
   * 
   * @throws IllegalStateException
   *           in case the tool isn't running.
   */
  public void cancelTool() throws IllegalStateException;

  /**
   * Called to invoke the actual tool.
   * @return TODO
   * 
   * @throws IllegalStateException
   *           in case the tool is already running.
   */
  public boolean invokeTool() throws IllegalStateException;

  /**
   * Resets the dialog, will be called if the dialog is shown at least once and
   * should be redisplayed again. Use this method to reset the state of the
   * dialog to its initial state.
   */
  public void reset();

  /**
   * Shows the dialog with the given capture results.
   */
  public void showDialog();

}
