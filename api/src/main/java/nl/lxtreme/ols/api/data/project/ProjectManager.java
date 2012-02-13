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
package nl.lxtreme.ols.api.data.project;


import java.beans.*;
import java.io.*;


/**
 * Provides a way of loading and storing projects.
 */
public interface ProjectManager
{
  // METHODS
  /**
   * Adds the given listener to the list of property change listeners.
   * 
   * @param aListener
   *          a property change listener, cannot be <code>null</code>.
   */
  public void addPropertyChangeListener( final PropertyChangeListener aListener );

  /**
   * Creates a new project.
   * 
   * @return a new project, managed by this manager.
   */
  public Project createNewProject();

  /**
   * Creates a temporary project, useful for loading data files.
   * 
   * @return a new project, <b>not</b> managed by this manager!
   * @deprecated do not use, no replacement.
   */
  @Deprecated
  public Project createTemporaryProject();

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
   * @throws IOException
   *           in case of I/O problems during the read of the project.
   */
  public void loadProject( final InputStream aInput ) throws IOException;

  /**
   * Removes the given listener from the list of property change listeners.
   * 
   * @param aListener
   *          a property change listener, cannot be <code>null</code>.
   */
  public void removePropertyChangeListener( final PropertyChangeListener aListener );

  /**
   * Stores a project to the given output stream.
   * 
   * @param aOutput
   *          the output to write the project to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during writing of the project.
   */
  public void saveProject( final OutputStream aOutput ) throws IOException;
}
