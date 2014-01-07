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
package nl.lxtreme.ols.client2.project;


import java.io.*;

import nl.lxtreme.ols.common.session.*;


/**
 * Provides a way of loading and storing projects.
 */
public interface ProjectManager
{
  // METHODS

  /**
   * Creates a new project.
   * 
   * @return a new project, managed by this manager.
   */
  void createNewProject();

  /**
   * @return the current project file, if defined, otherwise <code>null</code>.
   */
  File getProjectFile();

  /**
   * @return <code>true</code> if the current project is changed,
   *         <code>false</code> otherwise.
   */
  boolean isProjectChanged();

  /**
   * Loads acquired data from a given file.
   * 
   * @param aFile
   *          the input file to read the data from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the read of the project.
   */
  void loadDataFile( File aFile ) throws IOException;

  /**
   * Loads a project from a given file.
   * 
   * @param aFile
   *          the input file to read the project from, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during the read of the project.
   */
  void loadProject( File aFile ) throws IOException;

  /**
   * Stores the current acquisition data to the given file.
   * 
   * @param aFile
   *          the output file to write the data to, cannot be <code>null</code>;
   * @param aSession
   *          the session data to save to a data file, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during writing of the project.
   */
  void saveDataFile( File aFile, Session aSession ) throws IOException;

  /**
   * Stores a project to the given file.
   * 
   * @param aFile
   *          the output file to write the project file to, cannot be
   *          <code>null</code>;
   * @param aSession
   *          the session data to save to a project file, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems during writing of the project.
   */
  void saveProject( File aFile, Session aSession ) throws IOException;
}
