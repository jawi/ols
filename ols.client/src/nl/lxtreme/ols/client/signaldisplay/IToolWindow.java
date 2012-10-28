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
package nl.lxtreme.ols.client.signaldisplay;


import javax.swing.*;


/**
 * Denotes a window that can be docked.
 */
public interface IToolWindow
{
  // METHODS

  /**
   * Returns an icon for this tool window.
   * 
   * @return an optional icon for this tool window, can be <code>null</code> if
   *         no icon is desired.
   */
  Icon getIcon();

  /**
   * Returns an unique identifier for this tool window.
   * 
   * @return the identifier of this tool window, cannot be <code>null</code>.
   */
  String getId();

  /**
   * Returns a displayable name for this tool window.
   * 
   * @return the display name of this tool window, can be <code>null</code>.
   */
  String getName();
}
