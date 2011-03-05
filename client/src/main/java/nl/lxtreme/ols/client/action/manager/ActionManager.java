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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.action.manager;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.client.action.*;


/**
 * Provides a default implementation of {@link IActionManager}.
 */
public final class ActionManager implements IActionManager
{
  // VARIABLES

  private final Map<String, IManagedAction> registry;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ActionManager} instance.
   */
  ActionManager()
  {
    this.registry = new ConcurrentHashMap<String, IManagedAction>();
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.client.action.manager.IActionManager#add(nl.lxtreme.ols.client.action.IManagedAction)
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
   * @see nl.lxtreme.ols.client.action.manager.IActionManager#getAction(java.lang.String)
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
   * @see nl.lxtreme.ols.client.action.manager.IActionManager#remove(nl.lxtreme.ols.client.action.IManagedAction)
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
