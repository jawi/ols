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
import javax.microedition.io.*;

import org.osgi.service.io.*;
import org.osgi.service.log.*;

import purejavacomm.*;


/**
 * Provides a connection factory for serial devices.
 */
public class CommConnectionFactory implements ConnectionFactory
{
  // CONSTANTS

  /**
   * The scheme we're exposing through this factory. Serial URIs should be
   * written in the form of:
   * <tt>comm:COM2;baudrate=9600;bitsperchar=8;parity=none;stopbits=1</tt>.
   */
  public static final String SCHEME = "comm";

  /**
   * Number of tries before bailing out on establishing a connection to the
   * serial port...
   */
  private static final int MAX_TRIES = 10;
  /** Name to use when connecting to the port... */
  private static final String CONNECT_ID = CommConnectionFactory.class.getSimpleName();

  // VARIABLES

  // Injected by dependency manager...
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
      final CommPortOptions options = new CommPortOptions( aName );

      final SerialPort port = obtainSerialPort( options );

      port.setSerialPortParams( options.getBaudrate(), options.getDatabits(), options.getStopbits(),
          options.getParityMode() );

      port.setFlowControlMode( options.getFlowControl() );
      if ( aTimeouts )
      {
        // A receive timeout allows us to better control blocking I/O, such as
        // read() from the serial port...
        port.enableReceiveTimeout( 100 );
      }
      // Taken from
      // <http://mailman.qbang.org/pipermail/rxtx/2010-September/7821768.html>
      port.setRTS( true );
      port.setDTR( options.isDTR() );

      final CommConnectionImpl connection = new CommConnectionImpl( port );

      // Some devices need some time to initialize after being opened for the
      // first time, see issue #34.
      final int openDelay = options.getOpenDelay();
      if ( openDelay > 0 )
      {
        Thread.sleep( openDelay );
      }

      return connection;
    }
    catch ( IllegalArgumentException exception )
    {
      throw new IOException( "Invalid URI!", exception );
    }
    catch ( UnsupportedCommOperationException exception )
    {
      throw new IOException( "Unsupported operation!", exception );
    }
    catch ( NoSuchPortException exception )
    {
      throw new IOException( "No such port!" );
    }
    catch ( InterruptedException exception )
    {
      throw new IOException( "Interrupted while opening port!" );
    }
  }

  /**
   * Returns the serial port instance.
   * 
   * @param aOptions
   *          the serial port options, cannot be <code>null</code>.
   * @return the serial port instance, never <code>null</code>.
   * @throws NoSuchPortException
   *           in case the desired port does not exist;
   * @throws PortInUseException
   *           in case the desired port is already in use;
   * @throws IOException
   *           in case of other I/O problems.
   */
  private SerialPort getSerialPort( final CommPortOptions aOptions ) throws NoSuchPortException, PortInUseException,
      IOException
  {
    final CommPortIdentifier commPortId = CommPortIdentifier.getPortIdentifier( aOptions.getPortName() );
    if ( commPortId.isCurrentlyOwned() && ( commPortId.getCurrentOwner() != CONNECT_ID ) )
    {
      throw new PortInUseException();
    }

    final CommPort commPort = commPortId.open( CONNECT_ID, 2000 );
    if ( !( commPort instanceof SerialPort ) )
    {
      throw new IOException( "Not a serial port?!" );
    }

    return ( SerialPort )commPort;
  }

  /**
   * Performs a best effort in trying to get a serial port instance by calling
   * {@link #getSerialPort(CommPortOptions)} a number of times before bailing
   * out.
   * <p>
   * Idea taken from: <a href=
   * "http://mailman.qbang.org/pipermail/rxtx/2010-September/7821768.html">this
   * posting</a> on the RxTx mailing list.
   * </p>
   * 
   * @param aOptions
   *          the serial port options, cannot be <code>null</code>.
   * @return the serial port instance, never <code>null</code>.
   * @throws NoSuchPortException
   *           in case the desired port does (really) not exist;
   * @throws IOException
   *           in case of other I/O problems.
   */
  private SerialPort obtainSerialPort( final CommPortOptions aOptions ) throws NoSuchPortException, IOException
  {
    int tries = MAX_TRIES;
    SerialPort port = null;

    while ( ( tries-- >= 0 ) && ( port == null ) )
    {
      try
      {
        port = getSerialPort( aOptions );
      }
      catch ( PortInUseException exception )
      {
        this.logService.log( LogService.LOG_DEBUG, "Port (still) in use!", exception );
      }
      catch ( NoSuchPortException exception )
      {
        this.logService.log( LogService.LOG_DEBUG, "No such port!", exception );
        // Immediately stop trying.
        tries = -1;
      }
    }

    if ( port == null )
    {
      throw new NoSuchPortException();
    }

    return port;
  }
}
