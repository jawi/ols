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
package nl.lxtreme.ols.client2.session;


import static nl.lxtreme.ols.tool.api.ToolConstants.*;
import static nl.lxtreme.ols.common.OlsConstants.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.common.session.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.event.*;
import org.osgi.service.log.*;


/**
 * Default implementation of {@link SessionProvider}.
 */
public class SessionProviderImpl implements SessionProvider, EventHandler
{
  // VARIABLES

  private final ConcurrentMap<Session, Component> sessions;
  private final AtomicInteger sessionIdCounter;
  // Injected by Felix DM...
  private volatile BundleContext context;
  private volatile LogService log;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SessionProviderImpl} instance.
   */
  public SessionProviderImpl()
  {
    this.sessions = new ConcurrentHashMap<Session, Component>();
    this.sessionIdCounter = new AtomicInteger( 0 );
  }

  // METHODS

  /**
   * @param aSession
   *          the session to add, cannot be <code>null</code>.
   */
  public void addSession( Session aSession )
  {
    Properties props = createSessionProps( aSession );

    DependencyManager dm = new DependencyManager( this.context );
    Component comp = dm.createComponent() //
        .setInterface( Session.class.getName(), props ) //
        .setImplementation( aSession ) //
        .setServiceProperties( props ) //
        .add( dm.createServiceDependency().setService( EventAdmin.class ).setRequired( true ) ) //
        .add( dm.createServiceDependency().setService( LogService.class ).setRequired( false ) );

    if ( this.sessions.putIfAbsent( aSession, comp ) == null )
    {
      dm.add( comp );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Session[] getSessions()
  {
    return this.sessions.keySet().toArray( new Session[this.sessions.size()] );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent( Event aEvent )
  {
    String topic = aEvent.getTopic();
    if ( TOPIC_ACQUISITION_COMPLETE.equals( topic ) )
    {
      AcquisitionData data = ( AcquisitionData )aEvent.getProperty( TAC_DATA );

      if ( this.log != null )
      {
        this.log.log( LogService.LOG_INFO, "Creating new session for acquired data..." );
      }

      addSession( new SessionImpl( this.sessionIdCounter.incrementAndGet(), this, data ) );
    }
    else if ( TOPIC_TOOL_FINISHED.equals( topic ) )
    {
      AcquisitionData data = ( AcquisitionData )aEvent.getProperty( TTF_DATA );
      if ( data != null )
      {
        if ( this.log != null )
        {
          this.log.log( LogService.LOG_INFO, "Creating new session for tool data..." );
        }

        addSession( new SessionImpl( this.sessionIdCounter.incrementAndGet(), this, data ) );
      }
    }
    else if ( TOPIC_DATA_LOADED.equals( topic ) )
    {
      AcquisitionData data = ( AcquisitionData )aEvent.getProperty( TDL_DATA );
      String name = ( String )aEvent.getProperty( TDL_NAME );
      File file = ( File )aEvent.getProperty( TDL_FILE );

      if ( this.log != null )
      {
        this.log.log( LogService.LOG_INFO, "Creating new session for loaded data..." );
      }

      addSession( new SessionImpl( this.sessionIdCounter.incrementAndGet(), this, data, name, file ) );
    }
  }

  /**
   * @param aSession
   *          the session to remove, cannot be <code>null</code>.
   */
  public void removeSession( Session aSession )
  {
    Component comp = this.sessions.remove( aSession );
    if ( comp != null )
    {
      DependencyManager dm = comp.getDependencyManager();
      dm.remove( comp );
    }
  }

  private Properties createSessionProps( Session aSession )
  {
    Properties props = new Properties();
    props.put( "id", aSession.getId() );
    props.put( "time", System.currentTimeMillis() );
    return props;
  }
}
