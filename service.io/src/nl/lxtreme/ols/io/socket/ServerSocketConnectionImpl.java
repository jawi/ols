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


/**
 * Provides a {@link ServerSocketConnection} implementation for remote socket
 * connections.
 */
final class ServerSocketConnectionImpl implements ServerSocketConnection
{
  // VARIABLES

  private final ServerSocket socket;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ServerSocketConnectionImpl} instance.
   * 
   * @param aSocket
   *          the (server) socket to wrap, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given socket was <code>null</code>.
   */
  public ServerSocketConnectionImpl( final ServerSocket aSocket ) throws IllegalArgumentException
  {
    if ( aSocket == null )
    {
      throw new IllegalArgumentException( "Socket cannot be null!" );
    }
    this.socket = aSocket;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public StreamConnection acceptAndOpen() throws IOException
  {
    return new SocketConnectionImpl( this.socket.accept() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException
  {
    this.socket.close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLocalAddress() throws IOException
  {
    return this.socket.getInetAddress().getHostAddress();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLocalPort() throws IOException
  {
    return this.socket.getLocalPort();
  }

}
