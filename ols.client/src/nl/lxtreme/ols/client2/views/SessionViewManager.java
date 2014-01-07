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


import java.util.concurrent.*;

import nl.lxtreme.ols.client2.actionmanager.*;
import nl.lxtreme.ols.common.session.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * Provides a manager for session views.
 */
public class SessionViewManager
{
  // VARIABLES

  private final ConcurrentMap<Session, ViewController> sessions;
  private final ConcurrentMap<Session, Component> components;
  // Injected by Felix DM...
  private volatile BundleContext context;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SessionViewManager} instance.
   */
  public SessionViewManager()
  {
    this.sessions = new ConcurrentHashMap<Session, ViewController>();
    this.components = new ConcurrentHashMap<Session, Component>();
  }

  // METHODS

  /**
   * Adds a given session to this manager.
   * 
   * @param aSession
   *          the session to add, cannot be <code>null</code>.
   */
  public void addSession( Session aSession )
  {
    ViewController ctrl = new ViewController( aSession );
    if ( this.sessions.putIfAbsent( aSession, ctrl ) == null )
    {
      // New controller...
      DependencyManager dm = new DependencyManager( this.context );
      Component comp = dm.createComponent() //
          .setInterface( ViewController.class.getName(), null ) //
          .setImplementation( ctrl ) //
          .add( dm.createServiceDependency() //
              .setService( ActionManager.class ) //
              .setRequired( true ) ) //
          .add( dm.createServiceDependency() //
              .setService( LogService.class ) //
              .setRequired( false ) ) //
      ;

      if ( this.components.putIfAbsent( aSession, comp ) == null )
      {
        dm.add( comp );
      }
    }
  }

  /**
   * Removes a given session from this manager.
   * 
   * @param aSession
   *          the session to remove, cannot be <code>null</code>.
   */
  public void removeSession( Session aSession )
  {
    ViewController controller = this.sessions.remove( aSession );
    if ( controller != null )
    {
      Component comp = this.components.remove( aSession );
      if ( comp != null )
      {
        DependencyManager dm = comp.getDependencyManager();
        dm.remove( comp );
      }
    }
  }
}
