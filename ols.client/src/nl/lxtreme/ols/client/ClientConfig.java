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

  @Meta.AD( name = "Mouse wheel zooms?", deflt = "false" )
  boolean mouseWheelZooms();

  @Meta.AD( name = "Snap cursor to edge?", deflt = "false" )
  boolean snapCursorToEdge();

  @Meta.AD( name = "Show group summary?", deflt = "false" )
  boolean showGroupSummary();

  @Meta.AD( name = "Show analog scope?", deflt = "false" )
  boolean showAnalogScope();

  @Meta.AD( name = "Show channel indexes?", deflt = "false" )
  boolean showChannelIndexes();

  @Meta.AD( name = "Retain annotations?", deflt = "false" )
  boolean retainAnnotations();

  @Meta.AD( name = "Show tool windows?", deflt = "false" )
  boolean showToolWindows();

  @Meta.AD( name = "Signal alignment", optionLabels = { "Top", "Center", "Bottom" }, deflt = "CENTER" )
  SignalAlignment signalAlignment();

  @Meta.AD( name = "Annotation alignment", optionLabels = { "Top", "Center", "Bottom" }, deflt = "TOP" )
  SignalAlignment annotationAlignment();

  @Meta.AD( name = "Channel height", deflt = "36" )
  int channelHeight();

  @Meta.AD( name = "Signal height", deflt = "22" )
  int signalHeight();

  @Meta.AD( name = "Group summary height", deflt = "30" )
  int groupSummaryHeight();

  @Meta.AD( name = "Analog scope height", deflt = "96" )
  int analogScopeHeight();

}
