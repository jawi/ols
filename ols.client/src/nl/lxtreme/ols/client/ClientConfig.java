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


import nl.lxtreme.ols.client.signaldisplay.SignalDiagramModel.SignalAlignment;
import aQute.bnd.annotation.metatype.*;


/**
 * Provides the configuration metadata for the main client.
 */
@Meta.OCD( name = "Preferences" )
public interface ClientConfig
{
  // METHODS

  @Meta.AD( name = "Mouse wheel zooms?", deflt = "false", description = "Whether the mouse wheel by default zooms in or out, or scrolls the view. Will be applied immediately." )
  boolean mouseWheelZooms();

  @Meta.AD( name = "Snap cursor to edge?", deflt = "false", description = "Whether or not cursors by default snap to signal edges. Will be applied immediately." )
  boolean snapCursorToEdge();

  @Meta.AD( name = "Show group summary?", deflt = "false", description = "Whether or not the group (byte) summary is shown by default for each acquisition. Will be applied after an acquisition." )
  boolean showGroupSummary();

  @Meta.AD( name = "Show analog scope?", deflt = "false", description = "Whether or not the analog scope is shown by default for each acquisition. Will be applied after an acquisition." )
  boolean showAnalogScope();

  @Meta.AD( name = "Show channel indexes?", deflt = "false", description = "Whether or not channel indexes are shown beside the labels. Will be applied immediately." )
  boolean showChannelIndexes();

  @Meta.AD( name = "Retain annotations?", deflt = "false", description = "Whether or not annotations should be retained after a recapture. Will be applied immediately." )
  boolean retainAnnotations();

  @Meta.AD( name = "Show tool windows?", deflt = "false", description = "Whether or not the tool windows are shown by default. Will be applied after a restart." )
  boolean showToolWindows();

  @Meta.AD( name = "Signal alignment", optionLabels = { "Top", "Center", "Bottom" }, deflt = "CENTER", required = true, description = "The vertical alignment of the signals itself. Will be applied after an acquisition." )
  SignalAlignment signalAlignment();

  @Meta.AD( name = "Annotation alignment", optionLabels = { "Top", "Center", "Bottom" }, deflt = "CENTER", required = true, description = "The vertical aligment of the annotations. Will be applied immediately." )
  SignalAlignment annotationAlignment();

  @Meta.AD( name = "Channel height", deflt = "36", required = true, description = "The height (in pixels) of the channels." )
  int channelHeight();

  @Meta.AD( name = "Signal height", deflt = "22", required = true, description = "The height (in pixels) of the digital signals." )
  int signalHeight();

  @Meta.AD( name = "Group summary height", deflt = "30", required = true, description = "The height (in pixels) of the group summary." )
  int groupSummaryHeight();

  @Meta.AD( name = "Analog scope height", deflt = "96", required = true, description = "The height (in pixels) of the analog scope." )
  int analogScopeHeight();

  @Meta.AD( name = "Color scheme", deflt = "Default", required = true, description = "{managed=false}What color scheme is to be used. Will be applied immediately." )
  String colorScheme();

}
