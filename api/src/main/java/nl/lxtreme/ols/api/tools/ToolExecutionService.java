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
package nl.lxtreme.ols.api.tools;


import java.util.concurrent.*;


/**
 * Provides a service that can be used to invoke tools in a background thread.
 */
public interface ToolExecutionService
{
  // METHODS

  /**
   * Ensures that the given tool is invoked in the background.
   * 
   * @param aTool
   *          the tool to invoke, cannot be <code>null</code>.
   * @return a {@link Future} that can be used to control the execution of the
   *         tool, for example, to cancel its execution.
   */
  Future<Void> execute( Tool aTool );
}
