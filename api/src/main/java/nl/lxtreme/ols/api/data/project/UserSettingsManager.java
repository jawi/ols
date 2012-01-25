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
package nl.lxtreme.ols.api.data.project;


import java.io.*;


/**
 * Denotes a manager for user-settings, which are either implicit (e.g. window
 * size and placement), or explicit settings (e.g, settings explicitly made by
 * the user).
 */
public interface UserSettingsManager
{
  // METHODS

  /**
   * Loads the user settings for the given project.
   * 
   * @param aUserSettingsFile
   *          the file to load the user settings from, cannot be
   *          <code>null</code>;
   * @param aProject
   *          the project to load the user settings for, cannot be
   *          <code>null</code>.
   */
  void loadUserSettings( final File aUserSettingsFile, final Project aProject );

  /**
   * Saves the user settings for the given project.
   * 
   * @param aUserSettingsFile
   *          the file to write the user settings to, cannot be
   *          <code>null</code>;
   * @param aProject
   *          the project to save the user settings for, cannot be
   *          <code>null</code>.
   */
  void saveUserSettings( final File aUserSettingsFile, final Project aProject );

}
