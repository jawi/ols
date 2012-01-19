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
package nl.lxtreme.ols.io.socket;


import java.io.*;
import java.net.*;

import javax.microedition.io.*;

import org.osgi.service.io.*;
import org.osgi.service.log.*;


/**
 * Provides a connection factory for connecting to sockets.
 */
public class SocketConnectionFactory implements ConnectionFactory
{
  // CONSTANTS

  /**
   * The scheme we're exposing through this factory. Serial URIs should be
   * written in the form of: <tt>socket://127.0.0.1:1234;timeout=100</tt>.
   */
  public static final String SCHEME = "socket";

  // VARIABLES

  // Injected by DependencyManager...
  private volatile LogService logService;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Connection createConnection( final String aName, final int aMode, final boolean aTimeouts ) throws IOException
  {
    try
    {
      final SocketOptions options = new SocketOptions( aName );

      return createSocketConnection( options );
    }
    catch ( IllegalArgumentException exception )
    {
      throw new IOException( "Invalid URI!", exception );
    }
  }

  /**
   * Factory method for creating a new socket connection.
   * 
   * @param aOptions
   *          the options to use to connect to the socket, cannot be
   *          <code>null</code>.
   * @return a new {@link Connection}, never <code>null</code>.
   * @throws UnknownHostException
   *           in case we're trying to connect to an unknown host;
   * @throws IOException
   *           in case of other I/O problems;
   * @throws SocketException
   *           in case of errors in the underlying protocol.
   */
  private Connection createSocketConnection( final SocketOptions aOptions ) throws UnknownHostException, IOException,
      SocketException
  {
    if ( aOptions.isRemoteSocket() )
    {
      this.logService.log( LogService.LOG_DEBUG,
          "Opening socket to " + aOptions.getAddress() + ":" + aOptions.getPort() );

      Socket socket = new Socket( aOptions.getAddress(), aOptions.getPort() );
      socket.setSoTimeout( aOptions.getTimeout() );

      return new SocketConnectionImpl( socket );
    }

    this.logService.log( LogService.LOG_DEBUG, "Opening server socket on " + aOptions.getPort() );

    ServerSocket socket = new ServerSocket( aOptions.getPort() );
    socket.setSoTimeout( aOptions.getTimeout() );

    return new ServerSocketConnectionImpl( socket );
  }
}
