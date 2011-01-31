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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.rxtx;


import gnu.io.*;

import java.io.*;
import java.util.*;
import java.util.logging.*;

import javax.microedition.io.*;


/**
 * Provides a serial port connection.
 */
final class SerialConnection implements CommConnection, SerialPortEventListener
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( SerialConnection.class.getName() );

  // VARIABLES

  private final SerialPort port;

  private InputStream inputStream;
  private OutputStream outputStream;

  // CONSTRUCTORS

  /**
   * Creates a new SerialConnection instance.
   * 
   * @param aPort
   *          the serial port to wrap, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given port was <code>null</code>.
   */
  public SerialConnection( final SerialPort aPort ) throws IllegalArgumentException, IOException
  {
    if ( aPort == null )
    {
      throw new IllegalArgumentException( "Port cannot be null!" );
    }
    this.port = aPort;

    try
    {
      // We'll be listening for break interrupts, so we want to be notified of
      // these events...
      this.port.notifyOnBreakInterrupt( true );

      this.port.addEventListener( this );
    }
    catch ( TooManyListenersException exception )
    {
      throw new IOException( "Too many listeners registered for serial port?!" );
    }
  }

  // METHODS

  /**
   * @see javax.microedition.io.Connection#close()
   */
  @Override
  public void close() throws IOException
  {
    closeResource( this.inputStream );
    this.inputStream = null;
    closeResource( this.outputStream );
    this.outputStream = null;

    this.port.close();
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
    if ( this.inputStream == null )
    {
      this.inputStream = this.port.getInputStream();
    }
    else
    {
      LOG.warning( "Opening input stream twice?!" );
    }
    return this.inputStream;
  }

  /**
   * @see javax.microedition.io.OutputConnection#openOutputStream()
   */
  @Override
  public OutputStream openOutputStream() throws IOException
  {
    if ( this.outputStream == null )
    {
      this.outputStream = this.port.getOutputStream();
    }
    else
    {
      LOG.warning( "Opening output stream twice?!" );
    }
    return this.outputStream;
  }

  /**
   * @see gnu.io.SerialPortEventListener#serialEvent(gnu.io.SerialPortEvent)
   */
  @Override
  public void serialEvent( final SerialPortEvent aEvent )
  {
    final int type = aEvent.getEventType();
    if ( type == SerialPortEvent.BI )
    {
      LOG.log( Level.WARNING, "Serial connection unexpectedly shut down!" );

      try
      {
        close();
      }
      catch ( IOException exception )
      {
        LOG.log( Level.WARNING, "Error closing serial connection?! Port may remain in-use!", exception );
      }
    }
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
  private boolean closeResource( final Closeable aResource )
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
}
