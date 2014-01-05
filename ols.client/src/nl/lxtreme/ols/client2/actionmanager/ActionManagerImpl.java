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
package nl.lxtreme.ols.client2.actionmanager;


import java.util.*;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.*;
import nl.lxtreme.ols.client2.action.*;
import nl.lxtreme.ols.device.api.*;
import nl.lxtreme.ols.export.api.*;
import nl.lxtreme.ols.tool.api.*;

import org.osgi.framework.*;


/**
 * Listens to various service registrations and maintains corresponding
 * {@link Action}s for those services.
 */
public class ActionManagerImpl implements ActionManager
{
  // VARIABLES

  private final ConcurrentMap<String, ManagedAction> actions;
  private final ConcurrentMap<String, ServiceRegistration> serviceRegs;
  // Injected by Felix DM...
  private volatile BundleContext context;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ActionManagerImpl} instance.
   */
  public ActionManagerImpl()
  {
    this.actions = new ConcurrentHashMap<String, ManagedAction>();
    this.serviceRegs = new ConcurrentHashMap<String, ServiceRegistration>();
  }

  // METHODS

  static Dictionary<String, ?> toDictionary( Map<String, ?> aMap )
  {
    Hashtable<String, Object> result = new Hashtable<String, Object>();
    if ( aMap != null )
    {
      for ( Map.Entry<String, ?> entry : aMap.entrySet() )
      {
        result.put( entry.getKey(), entry.getValue() );
      }
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ManagedAction add( ManagedAction aAction ) throws IllegalArgumentException
  {
    String id = aAction.getId();
    if ( this.actions.putIfAbsent( id, aAction ) == null )
    {
      ServiceRegistration serviceReg = this.context.registerService( ManagedAction.class.getName(), aAction,
          toDictionary( aAction.getProperties() ) );

      this.serviceRegs.putIfAbsent( id, serviceReg );

      return aAction;
    }
    throw new IllegalArgumentException( "Action '" + id + "' already exists!" );
  }

  /**
   * Adds a given device to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to add, cannot be <code>null</code>.
   */
  public void addDevice( Device aDevice )
  {
    add( new SelectDeviceAction( aDevice ) );
  }

  /**
   * Adds a given exporter to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aExporter
   *          the exporter to add, cannot be <code>null</code>.
   */
  public void addExporter( final Exporter aExporter )
  {
    add( new ExportDataAction( aExporter ) );
  }

  /**
   * Adds a given tool to this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aTool
   *          the tool to add, cannot be <code>null</code>.
   */
  public void addTool( final Tool<?> aTool )
  {
    add( new InvokeToolAction( aTool ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ManagedAction getAction( String aId ) throws IllegalArgumentException
  {
    return this.actions.get( aId );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ManagedAction[] getActionByType( Class<? extends ManagedAction> aActionType )
  {
    List<ManagedAction> result = new ArrayList<ManagedAction>();
    for ( ManagedAction action : this.actions.values() )
    {
      if ( aActionType.isInstance( action ) )
      {
        result.add( action );
      }
    }
    return result.toArray( new ManagedAction[result.size()] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove( ManagedAction aAction ) throws IllegalArgumentException
  {
    String id = aAction.getId();
    if ( !this.actions.remove( id, aAction ) )
    {
      throw new IllegalArgumentException( "Action '" + id + "' does not exist!" );
    }

    ServiceRegistration serviceReg = this.serviceRegs.remove( id );
    if ( serviceReg != null )
    {
      serviceReg.unregister();
    }
  }

  /**
   * Removes a given device from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aDevice
   *          the device to remove, cannot be <code>null</code>.
   */
  public void removeDevice( Device aDevice )
  {
    ManagedAction action = getAction( SelectDeviceAction.getID( aDevice ) );
    if ( action != null )
    {
      remove( action );
    }
  }

  /**
   * Removes a given exporter from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aExporter
   *          the exporter to remove, cannot be <code>null</code>.
   */
  public void removeExporter( Exporter aExporter )
  {
    ManagedAction action = getAction( ExportDataAction.getID( aExporter ) );
    if ( action != null )
    {
      remove( action );
    }
  }

  /**
   * Removes a given tool from this controller.
   * <p>
   * This method is called by the dependency manager.
   * </p>
   * 
   * @param aTool
   *          the tool to remove, cannot be <code>null</code>.
   */
  public void removeTool( final Tool<?> aTool )
  {
    ManagedAction action = getAction( InvokeToolAction.getID( aTool ) );
    if ( action != null )
    {
      remove( action );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState( Client aClient )
  {
    for ( ManagedAction action : this.actions.values() )
    {
      action.updateState( aClient );
    }
  }
}
