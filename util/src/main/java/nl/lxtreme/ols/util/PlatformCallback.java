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
package nl.lxtreme.ols.util;


import java.io.*;
import java.util.*;


/**
 * Provides hooks into the native platform.
 */
public interface PlatformCallback
{
  // METHODS

  /**
   * Called to show the about box.
   */
  void handleAbout();

  /**
   * Called to handle "re-open" events for the application.
   */
  void handleAppReOpened();

  /**
   * Called when a list of files should be opened.
   *
   * @param aFileList
   *          the list of files to open.
   */
  void handleOpenFiles( List<File> aFileList );

  /**
   * Called to show the preferences dialog.
   */
  void handlePreferences();

  /**
   * @return <code>true</code> if the application should quit,
   *         <code>false</code> otherwise.
   */
  boolean shouldQuit();
}
