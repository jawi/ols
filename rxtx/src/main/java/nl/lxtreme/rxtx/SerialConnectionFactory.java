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
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.util.*;

import org.osgi.service.io.*;


/**
 * Provides a connection factory for serial devices.
 */
public class SerialConnectionFactory implements ConnectionFactory
{
  // CONSTANTS

  /**
   * The scheme we're exposing through this factory. Serial URIs should be
   * written in the form of:
   * <tt>comm:COM2;baudrate=9600;bitsperchar=8;parity=none;stopbits=1</tt>.
   */
  public static final String SCHEME = "comm";

  private static final Logger LOG = Logger.getLogger( SerialConnectionFactory.class.getName() );

  /**
   * Number of tries before bailing out on establishing a connection to the
   * serial port...
   */
  private static final int MAX_TRIES = 10;
  /** Name to use when connecting to the port... */
  private static final String CONNECT_ID = SerialConnectionFactory.class.getSimpleName();

  static
  {
    // For some reason, serial ports under Linux do not get properly enumerated.
    // This is due the fact that ttyACM is ignored in the detection routines of
    // RxTx. Therefore, the device won't appear in the list of devices, and also
    // cannot be entered manually as RxTx will refuse to add that particular
    // comm.port identifier.
    //
    // The workaround is to craft a serial ports path ourselves, and set the
    // system property 'gnu.io.rxtx.SerialPorts' ourselves with the "correct"
    // list of ports...
    // Reported by frankalicious on February 6th, 2011.
    try
    {
      final String portsEnum = CommPortUtils.enumerateDevices();
      if ( ( portsEnum != null ) && !portsEnum.trim().isEmpty() )
      {
        System.setProperty( "gnu.io.rxtx.SerialPorts", portsEnum );
      }
    }
    catch ( UnsupportedOperationException exception )
    {
      LOG.log( Level.WARNING, "Enumeration of serial devices failed! Proceeding anyway..." );
      LOG.log( Level.FINE, "Detailed stacktrace:", exception );
    }
  }

  // CONSTRUCTORS

  /**
   * Creates a new SerialConnectionFactory instance.
   */
  public SerialConnectionFactory()
  {
    final RXTXCommDriver driver = new RXTXCommDriver();
    driver.initialize();
  }

  // METHODS

  /**
   * @see org.osgi.service.io.ConnectionFactory#createConnection(java.lang.String,
   *      int, boolean)
   */
  @Override
  public Connection createConnection( final String aName, final int aMode, final boolean aTimeouts ) throws IOException
  {
    final SerialPortOptions options = new SerialPortOptions( aName );

    try
    {
      final SerialPort port = obtainSerialPort( options );

      port.setSerialPortParams( options.getBaudrate(), options.getDatabits(), options.getStopbits(),
          options.getParityMode() );

      port.setFlowControlMode( options.getFlowControl() );
      // A receive timeout allows us to better control blocking I/O, such as
      // read() from the serial port...
      port.enableReceiveTimeout( 50 );
      // Taken from
      // <http://mailman.qbang.org/pipermail/rxtx/2010-September/7821768.html>
      port.setRTS( true );
      port.setDTR( options.isDTR() );

      return new SerialConnection( port );
    }
    catch ( UnsupportedCommOperationException exception )
    {
      throw new IOException( "Unsupported operation!", exception );
    }
    catch ( NoSuchPortException exception )
    {
      throw new IOException( "No such port!" );
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
  private SerialPort getSerialPort( final SerialPortOptions aOptions ) throws NoSuchPortException, PortInUseException,
      IOException
  {
    final CommPortIdentifier commPortId = CommPortIdentifier.getPortIdentifier( aOptions.getPortName() );
    if ( commPortId.isCurrentlyOwned() )
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
   * {@link #getSerialPort(SerialPortOptions)} a number of times before bailing
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
  private SerialPort obtainSerialPort( final SerialPortOptions aOptions ) throws NoSuchPortException, IOException
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
        LOG.log( Level.FINE, "Port (still) in use!", exception );
      }
      catch ( NoSuchPortException exception )
      {
        LOG.log( Level.FINE, "No such port!", exception );
        // Immediately stop trying. On non-Windows platforms, try an alternative
        // approach as last resort...
        tries = -1;
      }
    }

    // A workaround for all non-Windows platforms: it could be that the device
    // name is not in the list of searched port-names, so we should try whether
    // the port itself can be opened directly. We should consider this a
    // best-effort strategy...
    if ( ( port == null ) && !HostUtils.isWindows() )
    {
      try
      {
        port = new RXTXPort( aOptions.getPortName() );
      }
      catch ( PortInUseException exception )
      {
        LOG.log( Level.FINE, "Port (still) in use!", exception );
      }
    }

    if ( port == null )
    {
      throw new NoSuchPortException();
    }

    return port;
  }
}
