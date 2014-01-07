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


import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import nl.lxtreme.ols.common.session.*;
import nl.lxtreme.ols.common.acquisition.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * Default implementation of {@link SessionProvider}.
 */
public class SessionProviderImpl implements SessionProvider, AcquisitionDataListener
{
  // VARIABLES

  private final ConcurrentMap<Session, ServiceRegistration> sessions;
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
    this.sessions = new ConcurrentHashMap<Session, ServiceRegistration>();
    this.sessionIdCounter = new AtomicInteger( 1 );
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void acquisitionComplete( AcquisitionData aData )
  {
    if ( this.log != null )
    {
      this.log.log( LogService.LOG_INFO, "Creating new session for acquired data..." );
    }
    addSession( new SessionImpl( this.sessionIdCounter.getAndIncrement(), this, aData ) );
  }

  /**
   * @param aSession
   *          the session to add, cannot be <code>null</code>.
   */
  public void addSession( Session aSession )
  {
    Properties props = createSessionProps( aSession );

    ServiceRegistration serviceReg = this.context.registerService( Session.class.getName(), aSession, props );

    this.sessions.putIfAbsent( aSession, serviceReg );
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
   * @param aSession
   *          the session to remove, cannot be <code>null</code>.
   */
  public void removeSession( Session aSession )
  {
    ServiceRegistration serviceReg = this.sessions.remove( aSession );
    if ( serviceReg != null )
    {
      serviceReg.unregister();
    }
  }

  private Properties createSessionProps( Session aSession )
  {
    Properties props = new Properties();
    props.put( "time", System.currentTimeMillis() );
    return props;
  }
}
