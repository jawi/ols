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
package nl.lxtreme.ols.client.actionmanager;


import java.util.*;
import java.util.concurrent.*;


/**
 * Provides a default implementation of {@link IActionManager}.
 */
public final class ActionManager implements IActionManager
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
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
  public IManagedAction[] getActionByType( final Class<? extends IManagedAction> aActionType )
  {
    final List<IManagedAction> actions = new ArrayList<IManagedAction>();
    for ( IManagedAction action : this.registry.values() )
    {
      if ( aActionType.isAssignableFrom( action.getClass() ) )
      {
        actions.add( action );
      }
    }
    return actions.toArray( new IManagedAction[actions.size()] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove( final IManagedAction aAction ) throws IllegalArgumentException
  {
    if ( aAction == null )
    {
      throw new IllegalArgumentException( "Action cannot be null!" );
    }

    final String id = aAction.getId();
    if ( !this.registry.containsKey( id ) )
    {
      throw new IllegalArgumentException( "Action " + id + " is not registered?!" );
    }

    this.registry.remove( id );
  }

}

/* EOF */
