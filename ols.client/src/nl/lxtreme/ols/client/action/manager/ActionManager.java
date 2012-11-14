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
package nl.lxtreme.ols.client.action.manager;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.util.swing.*;


/**
 * Provides a default implementation of {@link IActionManager}.
 */
public final class ActionManager
{
  // VARIABLES

  private final ConcurrentMap<String, IManagedAction> registry;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ActionManager} instance.
   */
  public ActionManager()
  {
    this.registry = new ConcurrentHashMap<String, IManagedAction>();
  }

  // METHODS

  /**
   * Adds a given action to this manager.
   * 
   * @param aAction
   *          the action to manage, cannot be <code>null</code> and should not
   *          already be registered.
   * @return the added action, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given action was <code>null</code> or already managed
   *           by this manager.
   */
  public IManagedAction add( final IManagedAction aAction ) throws IllegalArgumentException
  {
    if ( aAction == null )
    {
      throw new IllegalArgumentException( "Action cannot be null!" );
    }

    final String id = aAction.getId();
    if ( ( id == null ) || id.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Action " + aAction.getClass().getName() + " has no valid ID?!" );
    }
    if ( this.registry.containsKey( id ) )
    {
      throw new IllegalArgumentException( "Action " + id + " already registered?!" );
    }
    //
    this.registry.put( id, aAction );

    return aAction;
  }

  /**
   * Returns a managed action by its ID.
   * 
   * @param aId
   *          the ID of the managed action to retrieve, cannot be
   *          <code>null</code>.
   * @return the managed action with the given ID, never <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given ID did not lead to a valid action, or in case
   *           the given ID was <code>null</code>.
   */
  public IManagedAction getAction( final String aId ) throws IllegalArgumentException
  {
    if ( ( aId == null ) || aId.trim().isEmpty() )
    {
      throw new IllegalArgumentException( "Managed action ID cannot be null or empty!" );
    }

    final IManagedAction result = this.registry.get( aId );
    if ( result == null )
    {
      throw new IllegalArgumentException( "No such managed action: " + aId );
    }

    return result;
  }

  /**
   * Removes a given action from this manager.
   * 
   * @param aAction
   *          the action to remove, cannot be <code>null</code> and should be
   *          registered.
   * @throws IllegalArgumentException
   *           in case the given action was <code>null</code>.
   */
  public void remove( final IManagedAction aAction ) throws IllegalArgumentException
  {
    if ( aAction == null )
    {
      throw new IllegalArgumentException( "Action cannot be null!" );
    }

    this.registry.remove( aAction.getId(), aAction );
  }

  /**
   * Removes a given action from this manager.
   * 
   * @param aID
   *          the ID of the action to remove, cannot be <code>null</code> and
   *          should point to a registered action.
   * @throws IllegalArgumentException
   *           in case the given ID was <code>null</code>.
   */
  public void remove( final String aID ) throws IllegalArgumentException
  {
    if ( aID == null )
    {
      throw new IllegalArgumentException( "ID cannot be null!" );
    }

    this.registry.remove( aID );
  }

  /**
   * Removes all registered actions from this manager.
   */
  public void removeAll()
  {
    this.registry.clear();
  }

  /**
   * Updates the state of all managed actions.
   */
  public void updateActionStates()
  {
    final Collection<IManagedAction> values = this.registry.values();
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        for ( IManagedAction action : values )
        {
          try
          {
            action.updateState();
          }
          catch ( RuntimeException e )
          {
            e.printStackTrace();
            throw e;
          }
        }
      }
    } );
  }
}

/* EOF */
