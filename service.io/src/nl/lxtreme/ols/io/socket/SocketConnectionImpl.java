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
 * Provides a {@link SocketConnection} implementation for non-remote socket
 * connections.
 */
final class SocketConnectionImpl implements SocketConnection
{
  // CONSTANTS

  private static final int YES = 1;
  private static final int NO = 0;

  // VARIABLES

  private final Socket socket;

  // CONSTRUCTORS

  /**
   * Creates a new SocketConnectionImpl instance.
   * 
   * @param aSocket
   *          the socket to wrap, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given socket was <code>null</code>.
   */
  public SocketConnectionImpl( final Socket aSocket ) throws IllegalArgumentException
  {
    if ( aSocket == null )
    {
      throw new IllegalArgumentException( "Parameter socket cannot be null!" );
    }
    this.socket = aSocket;
  }

  // METHODS

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
  public String getAddress() throws IOException
  {
    return this.socket.getInetAddress().getHostAddress();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLocalAddress() throws IOException
  {
    return this.socket.getLocalAddress().getHostAddress();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getLocalPort() throws IOException
  {
    return this.socket.getLocalPort();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPort() throws IOException
  {
    return this.socket.getPort();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getSocketOption( final byte aOption ) throws IllegalArgumentException, IOException
  {
    switch ( aOption )
    {
      case DELAY:
        return this.socket.getTcpNoDelay() ? YES : NO;
      case LINGER:
        return this.socket.getSoLinger();
      case KEEPALIVE:
        return this.socket.getKeepAlive() ? YES : NO;
      case RCVBUF:
        return this.socket.getReceiveBufferSize();
      case SNDBUF:
        return this.socket.getSendBufferSize();
      default:
        throw new IllegalArgumentException( "Illegal option: " + aOption + "!" );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DataInputStream openDataInputStream() throws IOException
  {
    return new DataInputStream( openInputStream() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DataOutputStream openDataOutputStream() throws IOException
  {
    return new DataOutputStream( openOutputStream() );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public InputStream openInputStream() throws IOException
  {
    return this.socket.getInputStream();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public OutputStream openOutputStream() throws IOException
  {
    return this.socket.getOutputStream();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setSocketOption( final byte aOption, final int aValue ) throws IllegalArgumentException, IOException
  {
    switch ( aOption )
    {
      case DELAY:
        this.socket.setTcpNoDelay( aValue != NO );
        break;
      case LINGER:
        this.socket.setSoLinger( aValue != NO, aValue );
        break;
      case KEEPALIVE:
        this.socket.setKeepAlive( aValue != NO );
        break;
      case RCVBUF:
        this.socket.setReceiveBufferSize( aValue );
        break;
      case SNDBUF:
        this.socket.setSendBufferSize( aValue );
        break;
      default:
        throw new IllegalArgumentException( "Illegal option: " + aOption + "!" );
    }
  }
}
