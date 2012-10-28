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
package nl.lxtreme.ols.io.serial;


import java.io.*;
import java.util.logging.*;

import javax.microedition.io.*;

import purejavacomm.*;


/**
 * Provides a serial port connection, making use the JavaComm API as defined by
 * Sun/Oracle.
 * 
 * @see http://download.oracle.com/docs/cd/E17802_01/products/products/javacomm/
 *      reference/api/index.html
 */
final class CommConnectionImpl implements CommConnection
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( CommConnectionImpl.class.getName() );

  // VARIABLES

  private volatile InputStream is;
  private volatile OutputStream os;

  private SerialPort port;

  // CONSTRUCTORS

  /**
   * Creates a new SerialConnection instance.
   * 
   * @param aPort
   *          the serial port to wrap, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given port was <code>null</code>.
   */
  public CommConnectionImpl( final SerialPort aPort ) throws IllegalArgumentException
  {
    if ( aPort == null )
    {
      throw new IllegalArgumentException( "Port cannot be null!" );
    }
    this.port = aPort;
  }

  // METHODS

  /**
   * Closes a given resource.
   * <p>
   * If the given resource also implements the {@link Flushable} interface, the
   * resource is flushed before being closed.
   * </p>
   * 
   * @param aResource
   *          the resource to close, can be <code>null</code>, it might already
   *          be closed.
   * @return <code>true</code> if the close operation succeeded,
   *         <code>false</code> if it is unsure whether it succeeded.
   */
  private static boolean closeResource( final Closeable aResource )
  {
    boolean result = false;
    if ( aResource != null )
    {
      try
      {
        if ( aResource instanceof Flushable )
        {
          ( ( Flushable )aResource ).flush();
        }
      }
      catch ( IOException exception )
      {
        // Ignore...
        LOG.log( Level.WARNING, "I/O exception during flush!", exception );
      }
      finally
      {
        try
        {
          aResource.close();
          // Indicate success...
          result = true;
        }
        catch ( IOException exception )
        {
          // Ignore...
          LOG.log( Level.WARNING, "I/O exception during close!", exception );
        }
      }

    }
    return result;
  }

  /**
   * @see javax.microedition.io.Connection#close()
   */
  @Override
  public void close() throws IOException
  {
    try
    {
      closeResource( this.is );
      closeResource( this.os );

      if ( this.port != null )
      {
        this.port.close();
      }
    }
    finally
    {
      this.is = null;
      this.os = null;
      this.port = null;
    }
  }

  /**
   * @see javax.microedition.io.CommConnection#getBaudRate()
   */
  @Override
  public int getBaudRate()
  {
    return this.port.getBaudRate();
  }

  /**
   * @see javax.microedition.io.InputConnection#openDataInputStream()
   */
  @Override
  public DataInputStream openDataInputStream() throws IOException
  {
    return new DataInputStream( openInputStream() );
  }

  /**
   * @see javax.microedition.io.OutputConnection#openDataOutputStream()
   */
  @Override
  public DataOutputStream openDataOutputStream() throws IOException
  {
    return new DataOutputStream( openOutputStream() );
  }

  /**
   * @see javax.microedition.io.InputConnection#openInputStream()
   */
  @Override
  public InputStream openInputStream() throws IOException
  {
    if ( this.is != null )
    {
      return this.is;
    }
    return this.is = this.port.getInputStream();
  }

  /**
   * @see javax.microedition.io.OutputConnection#openOutputStream()
   */
  @Override
  public OutputStream openOutputStream() throws IOException
  {
    if ( this.os != null )
    {
      return this.os;
    }
    return this.os = this.port.getOutputStream();
  }

  /**
   * @see javax.microedition.io.CommConnection#setBaudRate(int)
   */
  @Override
  public int setBaudRate( final int aBaudRate )
  {
    final int oldBaudRate = getBaudRate();
    // not supported; no change possible...
    return oldBaudRate;
  }
}
