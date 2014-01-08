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
package nl.lxtreme.ols.client2.views;


/**
 * Provides a manager for views.
 */
public interface ViewManager
{
  // METHODS

  /**
   * Adds a given view to this manager.
   * 
   * @param aView
   *          the managed view, cannot be <code>null</code>.
   */
  ManagedView add( ManagedView aView );

  /**
   * Returns the managed view with the given identifier.
   * 
   * @param aId
   *          the identifier of the view to retrieve.
   * @return the view with the given identifier, or <code>null</code> if no such
   *         view is available.
   */
  ManagedView getView( String aId );

  /**
   * Removes a given view from this manager.
   * 
   * @param aView
   *          the managed view, cannot be <code>null</code>.
   */
  void remove( ManagedView aView );

  /**
   * Updates the state of all managed views.
   * 
   * @param aController
   */
  void updateState( ViewController aController );

}
