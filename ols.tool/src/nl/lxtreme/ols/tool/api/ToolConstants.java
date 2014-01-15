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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.tool.api;


import static nl.lxtreme.ols.common.OlsConstants.*;


/**
 * Constants used for the various {@link Tool}s.
 */
public interface ToolConstants
{
  // CONSTANTS

  /** Topic prefix used for all tool-related events. */
  String TOPIC_TOOL_BASE = TOPIC_DATA_BASE.concat( "/tool" );
  /** Topic where "tool started" events are posted. */
  String TOPIC_TOOL_STARTED = TOPIC_TOOL_BASE.concat( "/STARTED" );
  /** Topic where "tool started" events are posted. */
  String TOPIC_TOOL_PROGRESS = TOPIC_TOOL_BASE.concat( "/PROGRESS" );
  /** Topic where "tool finished" events are posted. */
  String TOPIC_TOOL_FINISHED = TOPIC_TOOL_BASE.concat( "/FINISHED" );

  /** The name of the tool that sent the {@link #TOPIC_TOOL_STARTED} event. */
  String TTS_TOOL_NAME = "toolName";

  /** The name of the tool that sent the {@link #TOPIC_TOOL_PROGRESS} event. */
  String TTP_TOOL_NAME = TTS_TOOL_NAME;
  /** The progress (integer value from 0..100) in a {@link #TOPIC_TOOL_PROGRESS} event. */
  String TTP_PROGRESS = "progress";
  
  /** The name of the tool that sent the {@link #TOPIC_TOOL_FINISHED} event. */
  String TTF_TOOL_NAME = TTP_TOOL_NAME;
  /** The total execution time of the tool in a {@link #TOPIC_TOOL_FINISHED} event. */
  String TTF_EXECUTION_TIME = "executionTime";
  /** The (optional) acquired data in a {@link #TOPIC_TOOL_FINISHED} event, in case the tool created new data. */
  String TTF_DATA = "data";
  /** The (optional) exception in a {@link #TOPIC_TOOL_FINISHED} event, in case the tool failed. */
  String TTF_EXCEPTION = "exception";

}
