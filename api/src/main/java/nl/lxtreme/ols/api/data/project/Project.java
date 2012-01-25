/*
 * OpenBench LogicSniffer / SUMP project 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General License for more details.
 *
 * You should have received a copy of the GNU General License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
 *
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.api.data.project;


import java.io.*;
import java.util.*;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.*;


/**
 * Denotes a project, which contains the settings of the entire client, along
 * with the last captured data.
 */
public interface Project
{
  // METHODS

  /**
   * Returns the current data set for this project.
   * 
   * @return a data set, can be <code>null</code> if there is no data yet.
   */
  DataSet getDataSet();

  /**
   * Returns the path to the project file.
   * 
   * @return a file object denoting the filename of this project, can be
   *         <code>null</code> in case this project is not yet saved.
   */
  File getFilename();

  /**
   * Returns the date on which the project is last saved.
   * 
   * @return the last modified date of this project, can be <code>null</code> if
   *         this project is not yet saved.
   */
  Date getLastModified();

  /**
   * Returns the name of this project.
   * 
   * @return the name of this project, or <code>null</code> if no name is yet
   *         given.
   */
  String getName();

  /**
   * Returns the other user settings, like UI-settings, and such.
   * 
   * @param aName
   *          the name of the user settings to retrieve, cannot be
   *          <code>null</code>.
   * @return a user settings object, never <code>null</code>.
   */
  UserSettings getSettings( final String aName );

  /**
   * Returns the version of the OLS-client that created this project.
   * 
   * @return a source version string, such as "0.9.2".
   */
  String getSourceVersion();

  /**
   * Returns whether the contents of this project is changed or not.
   * 
   * @return <code>true</code> if this project is changed, <code>false</code>
   *         otherwise.
   */
  boolean isChanged();

  /**
   * Allows data to be read from a given reader, any current data in this
   * project will be overwritten!
   * <p>
   * The format expected by this method is the
   * "<a href="https://github.com/jawi/
   * ols/wiki/OLS-data-file-format">OLS data-file format</a>".
   * </p>
   * 
   * @param aReader
   *          the reader to read from, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   */
  void readData( Reader aReader ) throws IOException;

  /**
   * Sets the captured data of this project.
   * 
   * @param aData
   *          the captured data, can be <code>null</code>.
   */
  void setCapturedData( final AcquisitionResult aData );

  /**
   * Marks this project as "changed".
   * 
   * @param aChanged
   *          <code>true</code> to mark this project as changed,
   *          <code>false</code> otherwise.
   */
  void setChanged( final boolean aChanged );

  /**
   * Sets the filename of this project.
   * 
   * @param aFilename
   *          the filename to set, can be <code>null</code>.
   */
  void setFilename( final File aFilename );

  /**
   * Sets the last modified date of this project.
   * 
   * @param aLastModified
   *          a last modified date, can be <code>null</code>.
   */
  void setLastModified( final Date aLastModified );

  /**
   * Sets the name of this project.
   * 
   * @param aName
   *          the name of this project, can be <code>null</code>.
   */
  void setName( final String aName );

  /**
   * Sets the user settings.
   * 
   * @param aSettings
   *          the user settings, cannot be <code>null</code>.
   */
  void setSettings( final UserSettings aSettings );

  /**
   * Sets the version of the OLS-client that saved this project.
   * 
   * @param aSourceVersion
   *          the source version, for example, "0.9.2", can be <code>null</code>
   *          .
   */
  void setSourceVersion( final String aSourceVersion );

  /**
   * Allows external callers to traverse the project structure without having to
   * know the exact implementation details of a project.
   * 
   * @param aVisitor
   *          the visitor callback to use, cannot be <code>null</code>.
   */
  void visit( final ProjectVisitor aVisitor );

  /**
   * Allows the current data to be written to a given writer. The format in
   * which the data will be written is the OLS data-file format.
   * 
   * @param aWriter
   *          the writer to write to, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems.
   * @see #readData(Reader)
   */
  void writeData( Writer aWriter ) throws IOException;
}
