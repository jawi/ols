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
package nl.lxtreme.ols.client.signaldisplay;


import java.util.*;

import nl.lxtreme.ols.api.data.*;


/**
 * Denotes a listener for cursor changes, such as setting, removing or moving
 * cursors.
 */
public interface ICursorChangeListener extends EventListener
{
  // CONSTANTS

  public static final String PROPERTY_TIMESTAMP = "timestamp";
  public static final String PROPERTY_COLOR = "color";
  public static final String PROPERTY_LABEL = "label";

  // METHODS

  /**
   * Called when a cursor is "added" on screen.
   * 
   * @param aCursor
   *          the cursor that is added, cannot be <code>null</code>.
   */
  void cursorAdded( Cursor aCursor );

  /**
   * Called when a property of a single cursor is changed/moved.
   * 
   * @param aPropertyName
   *          the name of the property that is changed, never <code>null</code>;
   * @param aOldCursor
   *          the old cursor, cannot be <code>null</code>;
   * @param aNewCursor
   *          the new cursor, cannot be <code>null</code>.
   */
  void cursorChanged( String aPropertyName, Cursor aOldCursor, Cursor aNewCursor );

  /**
   * Called when a single cursor is removed.
   * 
   * @param aOldCursor
   *          the old cursor (before removal), cannot be <code>null</code>.
   */
  void cursorRemoved( Cursor aOldCursor );

  /**
   * Called when the cursors are made invisible.
   */
  void cursorsInvisible();

  /**
   * Called when the cursors are made visible.
   */
  void cursorsVisible();
}
