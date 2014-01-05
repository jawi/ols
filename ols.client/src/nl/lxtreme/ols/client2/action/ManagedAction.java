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
package nl.lxtreme.ols.client2.action;


import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.*;


/**
 * Denotes an Swing {@link Action} that is managed by a framework as it is named
 * uniquely.
 */
public interface ManagedAction extends Action
{
  // CONSTANTS

  /**
   * String property to add when this managed action should be added to a menu
   * bar.
   */
  String MENU_NAME = "menuName";
  /**
   * Integer property to define the order in which the managed action should be
   * added.
   */
  String MENU_ORDER = "menuOrder";
  /**
   * Boolean property to define that the managed action should be part of a
   * {@link ButtonGroup}.
   */
  String MENU_GROUPED = "menuGrouped";
  /**
   * Boolean property to define that the managed action should be represented as
   * a checkbox.
   */
  String MENU_CHECKBOX = "menuCheckbox";
  /**
   * Boolean property to indicate that above this managed action, a separator
   * should be placed.
   */
  String MENU_SEPARATOR_ABOVE = "menuSeparatorAbove";
  /**
   * Boolean property to indicate that above this managed action, a separator
   * should be placed.
   */
  String MENU_SEPARATOR_BELOW = "menuSeparatorBelow";

  // METHODS

  /**
   * Returns the ID of this managed action.
   * 
   * @return an identifier of this managed action, never <code>null</code>.
   */
  String getId();

  /**
   * @return the properties of this managed action, can be <code>null</code>.
   */
  Map<String, ?> getProperties();

  /**
   * Updates the state of this action.
   * 
   * @param aClient
   *          the client to reflect the state to, cannot be <code>null</code>.
   */
  void updateState( Client aClient );
}

/* EOF */
