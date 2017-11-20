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
package nl.lxtreme.ols.io;


import java.io.*;
import java.net.*;
import java.util.*;
import javax.microedition.io.*;

import org.osgi.framework.*;
import org.osgi.service.io.*;
import org.osgi.service.log.*;


/**
 * Provides a implementation of the connector service.
 */
public class ConnectorServiceImpl implements ConnectorService
{
  // VARIABLES

  private final BundleContext context;
  private final List<Connection> openConnections;

  // Injected by DependencyManager...
  private volatile LogService logService;

  // CONSTRUCTORS

  /**
   * Creates a new ConnectorServiceImpl instance.
   *
   * @param aContext
   *          the bundle context to use, cannot be <code>null</code>.
   */
  public ConnectorServiceImpl( final BundleContext aContext )
  {
    this.context = aContext;
    this.openConnections = new ArrayList<Connection>();
  }

  // METHODS

  /**
   * Determines for the given URI the scheme and returns its corresponding
   * {@link Scheme} object.
   *
   * @param aURI
   *          the URI to get the scheme name for, cannot be <code>null</code>.
   * @return a scheme name, can be <code>null</code>.
   */
  static final String determineSchemeName( final String aURI )
  {
    String protocol = null;

    try
    {
      final URI uri = new URI( aURI );
      protocol = uri.getScheme();
    }
    catch ( final URISyntaxException exception )
    {
      final int index = aURI.indexOf( ':' );
      if ( index > 0 )
      {
        protocol = aURI.substring( 0, index );
      }
    }

    return protocol;
  }

  /**
   * Locates the connection factory service instance through OSGi.
   *
   * @param aContext
   *          the bundle context to use for communication with the OSGi
   *          framework, cannot be <code>null</code>;
   * @param aName
   *          the URI to parse, cannot be <code>null</code>.
   * @return the connection factory, or <code>null</code> if it could not be
   *         found.
   */
  static final ConnectionFactory getConnectionFactory( final BundleContext aContext, final String aName )
  {
    String protocol = determineSchemeName( aName );
    if ( protocol == null )
    {
      return null;
    }

    final String filter = "(".concat( ConnectionFactory.IO_SCHEME ).concat( "=" ).concat( protocol ).concat( ")" );
    try
    {
      final ServiceReference<?>[] serviceRefs = aContext.getAllServiceReferences( ConnectionFactory.class.getName(),
          filter );
      if ( ( serviceRefs != null ) && ( serviceRefs.length > 0 ) )
      {
        final ConnectionFactory result = ( ConnectionFactory )aContext.getService( serviceRefs[0] );
        return result;
      }
    }
    catch ( InvalidSyntaxException exception )
    {
      throw new InternalError( "Invalid service filter syntax?!" );
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Connection open( final String aName ) throws IOException
  {
    return open( aName, ConnectorService.READ_WRITE );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Connection open( final String aName, final int aMode ) throws IOException
  {
    return open( aName, aMode, false /* aTimeouts */ );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Connection open( final String aName, final int aMode, final boolean aTimeouts ) throws IOException
  {
    final ConnectionFactory cf = getConnectionFactory( this.context, aName );
    if ( cf == null )
    {
      throw new ConnectionNotFoundException( "No connection for: " + aName );
    }

    final Connection connection = cf.createConnection( aName, aMode, aTimeouts );
    synchronized ( this.openConnections )
    {
      this.openConnections.add( connection );
    }

    return connection;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DataInputStream openDataInputStream( final String aName ) throws IOException
  {
    return new DataInputStream( openInputStream( aName ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DataOutputStream openDataOutputStream( final String aName ) throws IOException
  {
    return new DataOutputStream( openOutputStream( aName ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public InputStream openInputStream( final String aName ) throws IOException
  {
    return openStreamConnection( aName ).openInputStream();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public OutputStream openOutputStream( final String aName ) throws IOException
  {
    return openStreamConnection( aName ).openOutputStream();
  }

  /**
   * Shuts down this connector service, closing all lingering connections.
   */
  public void shutdown()
  {
    final List<Connection> connections;
    synchronized ( this.openConnections )
    {
      connections = new ArrayList<Connection>( this.openConnections );
    }

    for ( Connection conn : connections )
    {
      try
      {
        conn.close();
      }
      catch ( IOException exception )
      {
        this.logService.log( LogService.LOG_DEBUG, "I/O exception closing connection!", exception );
      }
    }
  }

  /**
   * Helper method to open a stream connection for a given URI.
   *
   * @param aURI
   *          the URI to open a stream connection for, cannot be
   *          <code>null</code>.
   * @return a stream connection, never <code>null</code>.
   * @throws IOException
   *           in case the given URI is not supported, or in case of other I/O
   *           problems.
   */
  private StreamConnection openStreamConnection( final String aURI ) throws IOException
  {
    final Connection conn = open( aURI );
    if ( !( conn instanceof StreamConnection ) )
    {
      throw new IOException( "Streaming connection not supported for this protocol!" );
    }

    return ( StreamConnection )conn;
  }
}
