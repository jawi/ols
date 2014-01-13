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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views;


import java.util.*;

import nl.lxtreme.ols.common.acquisition.*;


/**
 * Implementations are notified when cursors are changed.
 */
public interface CursorChangeListener extends EventListener
{
  // INNER TYPES

  /**
   * Cursor change event.
   */
  public static class CursorChangeEvent
  {
    // VARIABLES

    private final String propertyName;
    private final Cursor oldValue;
    private final Cursor newValue;

    /**
     * Creates a new CursorChangeEvent instance.
     */
    public CursorChangeEvent( String aProperty, Cursor aOldValue, Cursor aNewValue )
    {
      this.propertyName = aProperty;
      this.oldValue = aOldValue;
      this.newValue = aNewValue;
    }

    /**
     * Returns the current value of propertyName.
     * 
     * @return the propertyName
     */
    public String getProperty()
    {
      return this.propertyName;
    }

    /**
     * Returns the current value of oldValue.
     * 
     * @return the oldValue
     */
    public Cursor getOldValue()
    {
      return this.oldValue;
    }

    /**
     * Returns the current value of newValue.
     * 
     * @return the newValue
     */
    public Cursor getNewValue()
    {
      return this.newValue;
    }
  }

  // CONSTANTS

  String PROPERTY_TIMESTAMP = "timestamp";
  String PROPERTY_COLOR = "color";
  String PROPERTY_LABEL = "label";

  // METHODS

  /**
   * Called when a cursor is changed.
   * 
   * @param aEvent
   *          the event, never <code>null</code>.
   */
  void cursorChanged( CursorChangeEvent aEvent );

}
