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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client;


import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.manager.*;
import nl.lxtreme.ols.client.tool.*;
import nl.lxtreme.ols.tool.api.*;

import org.osgi.service.log.*;


/**
 * Provides a front-end controller for dealing with {@link Tool}s.
 */
public final class ToolController
{
  // VARIABLES

  private final ConcurrentMap<String, ToolInvoker> tools;

  // Injected by Felix DM...
  private volatile LogService log;

  // CONSTRUCTORS

  /**
   * Creates a new ToolController instance.
   */
  public ToolController()
  {
    this.tools = new ConcurrentHashMap<String, ToolInvoker>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  public ToolInvoker getTool( final String aName )
  {
    if ( aName == null )
    {
      throw new IllegalArgumentException( "Name cannot be null!" );
    }
    return this.tools.get( aName );
  }

  /**
   * Returns all available tools.
   * 
   * @return an array of tool names, never <code>null</code>, but an empty array
   *         is possible.
   */
  public String[] getToolNames()
  {
    List<String> result = new ArrayList<String>( this.tools.keySet() );
    // Make sure we've got a predictable order of names...
    Collections.sort( result );

    return result.toArray( new String[result.size()] );
  }

  /**
   * Runs the tool denoted by the given name.
   * 
   * @param aToolName
   *          the name of the tool to run, cannot be <code>null</code>;
   * @param aParent
   *          the parent window to use, can be <code>null</code>.
   */
  public void invokeTool( final String aToolName, final Window aParent )
  {
    this.log.log( LogService.LOG_INFO, "Running tool: \"" + aToolName + "\" ..." );

    final ToolInvoker tool = getTool( aToolName );
    if ( tool == null )
    {
      this.log.log( LogService.LOG_WARNING, "No such tool: \"" + aToolName + "\" found!" );

      JOptionPane.showMessageDialog( aParent, "No such tool: " + aToolName, "Error ...", JOptionPane.ERROR_MESSAGE );
    }
    else
    {
      if ( tool.configure( aParent ) )
      {
        tool.invoke();
      }
    }
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
  final void addTool( final ToolInvoker aToolInvoker )
  {
    ActionManager actionManager = getActionManager();

    String name = aToolInvoker.getName();
    if ( this.tools.putIfAbsent( name, aToolInvoker ) == null )
    {
      this.log.log( LogService.LOG_INFO, "Added tool '" + name + "' ..." );

      actionManager.add( new RunToolAction( this, name, aToolInvoker.getCategory() ) );
    }
    else
    {
      this.log.log( LogService.LOG_INFO, "Add of tool '" + name + "' failed!" );
    }

    actionManager.updateActionStates();
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
  final void removeTool( final ToolInvoker aTool )
  {
    ActionManager actionManager = getActionManager();

    String name = aTool.getName();
    if ( this.tools.remove( name, aTool ) )
    {
      this.log.log( LogService.LOG_INFO, "Removed tool '" + name + "' ..." );

      actionManager.remove( RunToolAction.getID( name ) );
    }
    else
    {
      this.log.log( LogService.LOG_INFO, "Removal of tool '" + name + "' failed!" );
    }

    actionManager.updateActionStates();
  }

  /**
   * @return the current action manager, never <code>null</code>.
   */
  private ActionManager getActionManager()
  {
    return Client.getInstance().getActionManager();
  }
}
