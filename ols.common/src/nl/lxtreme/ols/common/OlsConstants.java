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
package nl.lxtreme.ols.common;


/**
 * Defines some bare constants, used throughout the rest of the application.
 */
public interface OlsConstants
{
  // METHODS

  /** indicates that rate or trigger position are not available */
  int NOT_AVAILABLE = -1;

  /** The maximum number of cursors that can be set. */
  int MAX_CURSORS = 10;

  /** The maximum number of channels. */
  int MAX_CHANNELS = 32;

  /** The number of channels per block. */
  int CHANNELS_PER_BLOCK = 8;

  /** The maximum number of blocks. */
  int MAX_BLOCKS = MAX_CHANNELS / CHANNELS_PER_BLOCK;

  /** Topic prefix used for all acquisition-related events. */
  String TOPIC_DATA_BASE = "nl/lxtreme/ols/data";
  
  /** Topic prefix used for all acquisition-related events. */
  String TOPIC_ACQUISITION_BASE = TOPIC_DATA_BASE.concat( "/acquisition" );
  /** Topic where "acquisition complete" events are posted. */
  String TOPIC_ACQUISITION_COMPLETE = TOPIC_ACQUISITION_BASE.concat( "/COMPLETE" );
  /** The acquired data in a {@link #TOPIC_ACQUISITION_COMPLETE} event. */
  String TAC_DATA = "data";

  /** Topic where "acquisition complete" events are posted. */
  String TOPIC_DATA_LOADED = TOPIC_DATA_BASE.concat( "/LOADED" );
  /** The acquired data in a {@link #TOPIC_DATA_LOADED} event. */
  String TDL_DATA = TAC_DATA;
  /** The (optional) name in a {@link #TOPIC_DATA_LOADED} event. */
  String TDL_NAME = "name";
  /** The (optional) file in a {@link #TOPIC_DATA_LOADED} event. */
  String TDL_FILE = "file";

}
