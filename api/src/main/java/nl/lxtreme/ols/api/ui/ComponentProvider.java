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
package nl.lxtreme.ols.api.ui;


import javax.swing.*;


/**
 * Service interface which provides an added function when the component is
 * used.
 */
public interface ComponentProvider
{
  // CONSTANTS

  /**
   * Constant to identify ComponentProvider services.
   */
  public static final String COMPONENT_ID_KEY = "component.id";

  /** Constant to provide a menu component. */
  public static final String MENU_COMPONENT = "MenuComponent";

  // METHODS

  /**
   * This function should always be called from the EDT. The implementor may
   * assume that this function is called once and before
   * {@link #addedToContainer()}
   * 
   * @return the implementors (Swing) component which it provides.
   */
  public JComponent getComponent();

  /**
   * Triggered when the component is added to a container. The implementation
   * can validate some stuff. This function must be called on the EDT.<br/>
   * Implementors may assume this function is called after
   * {@link #getComponent()}.
   */
  public void addedToContainer();

  /**
   * Triggered when the component is removed from a container. The
   * implementation can validate some stuff. This function must be called on the
   * EDT.<br/>
   * Implementors may assume this function is called after
   * {@link #getComponent()}.
   */
  public void removedFromContainer();
}

/* EOF */
