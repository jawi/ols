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
package nl.lxtreme.ols.client.project.impl;


/**
 * Marker interface with several constants useful to listen for property changes
 * on the project.
 */
public interface ProjectProperties
{
  // CONSTANTS

  /** The name of the project. */
  public static final String PROPERTY_NAME = "name";
  /** The filename of the project. */
  public static final String PROPERTY_FILENAME = "filename";
  /** The 'change' flag of the project. */
  public static final String PROPERTY_CHANGED = "changed";
  /** Whether or not cursors are enabled in the project. */
  public static final String PROPERTY_CURSORS_ENABLED = "cursorsEnabled";
  /** The cursor positions of this project. */
  public static final String PROPERTY_CURSORS = "cursors";
  /** The channel labels of this project. */
  public static final String PROPERTY_CHANNEL_LABELS = "channelLabels";
  /** The last modified timestamp of the project. */
  public static final String PROPERTY_LAST_MODIFIED = "lastModified";
  /** The client version that last modified the project. */
  public static final String PROPERTY_SOURCE_VERSION = "sourceVersion";
  /** The project settings. */
  public static final String PROPERTY_SETTINGS = "settings";
  /** The captured data of the project. */
  public static final String PROPERTY_CAPTURED_DATA = "capturedData";
}
