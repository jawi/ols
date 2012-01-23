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
   * Returns the acquisition data of this project.
   * 
   * @return a captured data, can be <code>null</code>.
   */
  public AcquisitionResult getCapturedData();

  /**
   * Returns a single channel.
   * 
   * @param aIndex
   *          the channel index, >= 0 && <
   *          {@link AcquisitionResult#getChannels()}.
   * @return an array of channels, never <code>null</code>.
   */
  public Channel getChannel( int aIndex );

  /**
   * Returns the channels of this project.
   * 
   * @return an array of channels, never <code>null</code>.
   */
  public Channel[] getChannels();

  /**
   * Returns a single cursor
   * 
   * @param aIndex
   *          the index of the cursor to retrieve, >= 0.
   */
  public Cursor getCursor( int aIndex );

  /**
   * Returns the available cursors of this project.
   * 
   * @return an array of cursors, never <code>null</code>.
   */
  public Cursor[] getCursors();

  /**
   * Returns the path to the project file.
   * 
   * @return a file object denoting the filename of this project, can be
   *         <code>null</code> in case this project is not yet saved.
   */
  public File getFilename();

  /**
   * Returns the date on which the project is last saved.
   * 
   * @return the last modified date of this project, can be <code>null</code> if
   *         this project is not yet saved.
   */
  public Date getLastModified();

  /**
   * Returns the name of this project.
   * 
   * @return the name of this project, or <code>null</code> if no name is yet
   *         given.
   */
  public String getName();

  /**
   * Returns the other user settings, like UI-settings, and such.
   * 
   * @param aName
   *          the name of the user settings to retrieve, cannot be
   *          <code>null</code>.
   * @return a user settings object, never <code>null</code>.
   */
  public UserSettings getSettings( final String aName );

  /**
   * Returns the version of the OLS-client that created this project.
   * 
   * @return a source version string, such as "0.9.2".
   */
  public String getSourceVersion();

  /**
   * Returns whether the contents of this project is changed or not.
   * 
   * @return <code>true</code> if this project is changed, <code>false</code>
   *         otherwise.
   */
  public boolean isChanged();

  /**
   * Returns whether or not cursors are enabled.
   * 
   * @return <code>true</code> if cursors are enabled, <code>false</code>
   *         otherwise.
   */
  public boolean isCursorsEnabled();

  /**
   * Sets the captured data of this project.
   * 
   * @param aData
   *          the captured data, can be <code>null</code>.
   */
  public void setCapturedData( final AcquisitionResult aData );

  /**
   * Marks this project as "changed".
   * 
   * @param aChanged
   *          <code>true</code> to mark this project as changed,
   *          <code>false</code> otherwise.
   */
  public void setChanged( final boolean aChanged );

  /**
   * Sets the channel labels.
   * 
   * @param aChannelLabels
   *          the channel labels, in order, can be <code>null</code>.
   */
  @Deprecated
  public void setChannels( final Channel... aChannels );

  /**
   * Sets the cursor positions.
   * 
   * @param aCursors
   *          the cursor positions to set, in order, can be <code>null</code>.
   */
  @Deprecated
  public void setCursors( final Cursor... aCursors );

  /**
   * Sets whether or not cursors are enabled.
   * 
   * @param aEnabled
   *          <code>true</code> if cursors are enabled, <code>false</code>
   *          otherwise.
   */
  public void setCursorsEnabled( final boolean aEnabled );

  /**
   * Sets the filename of this project.
   * 
   * @param aFilename
   *          the filename to set, can be <code>null</code>.
   */
  public void setFilename( final File aFilename );

  /**
   * Sets the last modified date of this project.
   * 
   * @param aLastModified
   *          a last modified date, can be <code>null</code>.
   */
  public void setLastModified( final Date aLastModified );

  /**
   * Sets the name of this project.
   * 
   * @param aName
   *          the name of this project, can be <code>null</code>.
   */
  public void setName( final String aName );

  /**
   * Sets the user settings.
   * 
   * @param aSettings
   *          the user settings, cannot be <code>null</code>.
   */
  public void setSettings( final UserSettings aSettings );

  /**
   * Sets the version of the OLS-client that saved this project.
   * 
   * @param aSourceVersion
   *          the source version, for example, "0.9.2", can be <code>null</code>
   *          .
   */
  public void setSourceVersion( final String aSourceVersion );

  /**
   * Allows external callers to traverse the project structure without having to
   * know the exact implementation details of a project.
   * 
   * @param aVisitor
   *          the visitor callback to use, cannot be <code>null</code>.
   */
  public void visit( final ProjectVisitor aVisitor );
}
