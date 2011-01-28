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
package nl.lxtreme.ols.api.data.project;


import java.io.*;


/**
 * Provides a way of loading and storing projects.
 */
public interface ProjectManager
{
  // METHODS

  /**
   * Creates a new project.
   */
  public void createNewProject();

  /**
   * Returns the current project.
   * 
   * @return a project, never <code>null</code>.
   */
  public Project getCurrentProject();

  /**
   * Loads a project from the given input stream.
   * 
   * @param aInput
   *          the input to read the project from, cannot be <code>null</code>.
   * @return the loaded project, never <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the read of the project.
   */
  public void loadProject( final InputStream aInput ) throws IOException;

  /**
   * Stores a project to the given output stream.
   * 
   * @param aProject
   *          the project to store, cannot be <code>null</code>;
   * @param aOutput
   *          the output to write the project to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during writing of the project.
   */
  public void saveProject( final OutputStream aOutput ) throws IOException;
}
