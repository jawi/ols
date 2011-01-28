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


import java.util.*;

import nl.lxtreme.ols.api.data.*;


/**
 * Denotes a project, which contains the settings of the entire client, along
 * with the last captured data.
 */
public interface Project
{
  // METHODS

  /**
   * Returns the captured data of this project.
   * 
   * @return a captured data, can be <code>null</code>.
   */
  public CapturedData getCapturedData();

  /**
   * Returns the channel labels of this project.
   * 
   * @return an array of channel labels, can be <code>null</code>.
   */
  public String[] getChannelLabels();

  /**
   * Returns the available cursors of this project.
   * 
   * @return an array of cursor positions, can be <code>null</code>.
   */
  public long[] getCursorPositions();

  /**
   * @return
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
   * Returns the other project settings, like UI-settings, and such.
   * 
   * @return a properties object, never <code>null</code>.
   */
  public Properties getSettings();

  /**
   * @return
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
   * @param aCapturedData
   */
  public void setCapturedData( final CapturedData aCapturedData );

  /**
   * @param aChanged
   */
  public void setChanged( final boolean aChanged );

  /**
   * @param aChannelLabels
   */
  public void setChannelLabels( final String... aChannelLabels );

  /**
   * @param aCursors
   */
  public void setCursorPositions( final long... aCursors );

  /**
   * @param aLastModified
   */
  public void setLastModified( final Date aLastModified );

  /**
   * @param aName
   */
  public void setName( final String aName );

  /**
   * @param aSettings
   */
  public void setSettings( final Properties aSettings );

  /**
   * @param aSourceVersion
   */
  public void setSourceVersion( final String aSourceVersion );
}
