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
package nl.lxtreme.ols.client;


import java.io.*;

import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.project.*;
import nl.lxtreme.ols.client.project.impl.*;
import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.ioutil.*;


/**
 * Provides a front-end controller for handling "native" data input and output.
 */
public final class ProjectController
{
  // VARIABLES

  private volatile Project project;

  // Injected by Felix DM...
  private volatile StatusListener statusListener;
  private volatile ActionManager actionManager;
  private volatile Session session;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ProjectController} instance.
   */
  public ProjectController()
  {
    this.project = new ProjectImpl();
  }

  // METHODS

  /**
   * Creates a new project.
   */
  public void createNewProject()
  {
    this.session.reset();

    this.project = new ProjectImpl();
  }

  /**
   * @return the current project, never <code>null</code>.
   */
  public Project getProject()
  {
    return this.project;
  }

  /**
   * Returns the file to the current project.
   * 
   * @return the {@link File}-object corresponding to the current project, can
   *         be <code>null</code> if the current project is not yet saved.
   */
  public File getProjectFile()
  {
    return this.project.getFile();
  }

  /**
   * Returns whether or not the current project is "anonymous", i.e., not yet
   * saved.
   * 
   * @return <code>true</code> if the current project is anonymous,
   *         <code>false</code> otherwise.
   */
  public boolean isAnonymousProject()
  {
    return getProjectFile() == null;
  }

  /**
   * Returns whether or not the current project is changed.
   * 
   * @return <code>true</code> if the current project is changed,
   *         <code>false</code> otherwise.
   */
  public boolean isProjectChanged()
  {
    return this.project.isChanged();
  }

  /**
   * Opens a given file as OLS-data file.
   * 
   * @param aFile
   *          the file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case opening/reading from the given file failed.
   */
  public void openDataFile( final File aFile ) throws IOException
  {
    final FileReader reader = new FileReader( aFile );

    try
    {
      this.project.readData( this.session, reader );

      this.statusListener.setStatus( "Capture data loaded from {0} ...", aFile.getName() );
    }
    finally
    {
      IOUtil.closeResource( reader );
      this.actionManager.updateActionStates();
    }
  }

  /**
   * Opens a given file as OLS-project file.
   * 
   * @param aFile
   *          the file to open, cannot be <code>null</code>.
   * @throws IOException
   *           in case opening/reading from the given file failed.
   */
  public void openProjectFile( final File aFile ) throws IOException
  {
    FileInputStream fis = null;

    try
    {
      fis = new FileInputStream( aFile );

      // this.projectManager.loadProject( fis );XXX

      final Project project = getProject();
      project.setFilename( aFile );

      this.statusListener.setStatus( "Project {0} loaded ...", project.getName() );
    }
    finally
    {
      IOUtil.closeResource( fis );
      this.actionManager.updateActionStates();
    }
  }

  /**
   * Stores the current acquisition data to the given file, in the OLS-data file
   * format.
   * 
   * @param aFile
   *          the file to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of errors during opening/writing to the file.
   */
  public void saveDataFile( final File aFile ) throws IOException
  {
    final FileWriter writer = new FileWriter( aFile );
    try
    {
      getProject().writeData( this.session, writer );

      this.statusListener.setStatus( "Capture data saved to {0} ...", aFile.getName() );
    }
    finally
    {
      IOUtil.closeResource( writer );
    }
  }

  /**
   * Stores the current acquisition data to the given file, in the OLS-project
   * file format.
   * 
   * @param aName
   *          the name of the project to store, cannot be <code>null</code>;
   * @param aFile
   *          the file to write the data to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of errors during opening/writing to the file.
   */
  public void saveProjectFile( final String aName, final File aFile ) throws IOException
  {
    FileOutputStream out = null;
    try
    {
      final Project project = getProject();
      project.setFilename( aFile );
      project.setName( aName );

      out = new FileOutputStream( aFile );
      // this.projectManager.saveProject( out );XXX

      this.statusListener.setStatus( "Project {0} saved ...", aName );
    }
    finally
    {
      IOUtil.closeResource( out );
    }
  }
}
