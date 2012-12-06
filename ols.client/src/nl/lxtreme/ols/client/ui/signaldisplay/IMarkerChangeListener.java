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
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.ui.signaldisplay;


import java.util.*;

import nl.lxtreme.ols.client.ui.signaldisplay.marker.*;


/**
 * Denotes a listener for marker changes, such as setting, removing or moving
 * markers.
 */
public interface IMarkerChangeListener extends EventListener
{
  // CONSTANTS

  public static final String PROPERTY_COLOR = "color";
  public static final String PROPERTY_LABEL = "label";

  // METHODS

  /**
   * Called when a marker is "added" on screen.
   * 
   * @param aMarker
   *          the marker that is added, cannot be <code>null</code>.
   */
  void markerAdded( Marker aMarker );

  /**
   * Called when a property of a single marker is changed/moved.
   * 
   * @param aPropertyName
   *          the name of the property that is changed, never <code>null</code>;
   * @param aNewMarker
   *          the new/changed marker, cannot be <code>null</code>.
   */
  void markerChanged( String aPropertyName, Marker aNewMarker );

  /**
   * Called when a marker is moved.
   * 
   * @param aOldTimestamp
   *          the old timestamp, cannot be <code>null</code>;
   * @param aNewTimestamp
   *          the new timestamp, cannot be <code>null</code>.
   */
  void markerMoved( long aOldTimestamp, long aNewTimestamp );

  /**
   * Called when a single marker is removed.
   * 
   * @param aOldMarker
   *          the old marker (before removal), cannot be <code>null</code>.
   */
  void markerRemoved( Marker aOldMarker );

  /**
   * Called when the cursors are made invisible.
   */
  void cursorsInvisible();

  /**
   * Called when the cursors are made visible.
   */
  void cursorsVisible();
}
