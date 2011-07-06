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
package nl.lxtreme.rxtx;


import gnu.io.*;

import java.io.*;
import java.util.logging.*;

import javax.microedition.io.*;


/**
 * Provides a serial port connection.
 */
final class SerialConnection implements CommConnection
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SerialConnection.class.getName() );

  // VARIABLES

  private RXTXPort port;

  // CONSTRUCTORS

  /**
   * Creates a new SerialConnection instance.
   * 
   * @param aPort
   *          the serial port to wrap, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given port was <code>null</code>.
   */
  public SerialConnection( final RXTXPort aPort ) throws IllegalArgumentException, IOException
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
    closeResource( this.port.getInputStream() );
    closeResource( this.port.getOutputStream() );

    this.port.close();
    this.port = null;
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
    return this.port.getInputStream();
  }

  /**
   * @see javax.microedition.io.OutputConnection#openOutputStream()
   */
  @Override
  public OutputStream openOutputStream() throws IOException
  {
    return this.port.getOutputStream();
  }

  /**
   * @see javax.microedition.io.CommConnection#setBaudRate(int)
   */
  @Override
  public int setBaudRate( final int aBaudRate )
  {
    final int oldBaudRate = getBaudRate();
    try
    {
      this.port.setBaudBase( aBaudRate );
    }
    catch ( UnsupportedCommOperationException exception )
    {
      // Ignore...
      LOG.log( Level.WARNING, "Setting the baud rate failed; baudrate is NOT changed!", exception );
    }
    catch ( IOException exception )
    {
      // Ignore...
      LOG.log( Level.WARNING, "Setting the baud rate failed; baudrate is NOT changed!", exception );
    }
    return oldBaudRate;
  }
}
