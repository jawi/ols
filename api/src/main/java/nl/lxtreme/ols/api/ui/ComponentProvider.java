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
 * Service interface which provides new components at runtime.
 * <p>
 * For now, the only components that can be provided are {@link JMenu}
 * instances, which are added in and removed from the main dialog's menu bar.
 * Other kinds of components can/will be supported in the future.
 * </p>
 * <p>
 * If the implementing class of this interface contains a method with the
 * signature <tt>#init(org.osgi.framework.BundleContext)</tt> (any access
 * modifier), it will be called automatically upon registration of this service.
 * This allows implementors to obtain the <tt>BundleContext</tt> of this service
 * and access other OSGi services in a low-level manner.
 * </p>
 */
public interface ComponentProvider
{
  // CONSTANTS

  /** Constant to identify ComponentProvider services. */
  public static final String COMPONENT_ID_KEY = "component.id";

  /** Constant to provide a menu component. */
  public static final String MENU_COMPONENT = "Menu";

  // METHODS

  /**
   * Triggered when the component is added to a container.
   * <p>
   * Implementors can use this method to initialize listeners and/or other
   * components in context of the (parent) container.
   * </p>
   * <p>
   * The implementation can validate some stuff. This function is always called
   * on the <em>Event Dispatch Thread</em> (EDT).<br/>
   * Implementors may assume this function is called once <em>after</em>
   * {@link #getComponent()}.
   * </p>
   */
  public void addedToContainer();

  /**
   * Returns the actual instance of the component provided by this
   * implementation.
   * <p>
   * Implementors of this interface should keep track of the returned components
   * themselves to access their context (e.g.: their parent). In other words,
   * implementors should return exactly <em>one</em> instance of the component
   * provided by their implementation.
   * </p>
   * <p>
   * This function is always be called on the <em>Event Dispatch Thread</em>
   * (EDT). The implementor may assume that this function is called once before
   * {@link #addedToContainer()} , but afterwards, this method can be called
   * multiple times.
   * </p>
   * 
   * @return the (Swing) component provided by this service, cannot be
   *         <code>null</code>.
   */
  public JComponent getComponent();

  /**
   * Triggered when the component is about to get removed from a container.
   * <p>
   * Implementors can use this method to remove listeners and/or other
   * components in context of the (parent) container.
   * </p>
   * <p>
   * The implementation can validate some stuff. This function <b>must</b> be
   * called on the <em>Event Dispatch Thread</em> (EDT).<br/>
   * Implementors may assume this function is called once <em>after</em>
   * {@link #getComponent()}.
   * </p>
   */
  public void removedFromContainer();
}
