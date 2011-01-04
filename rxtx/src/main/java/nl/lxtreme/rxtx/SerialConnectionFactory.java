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

import javax.microedition.io.*;

import org.osgi.service.io.*;


/**
 * Provides a connection factory for serial devices.
 */
public class SerialConnectionFactory implements ConnectionFactory
{
  // INNER TYPES

  /**
   * The scheme we're exposing through this factory. Serial URIs should be
   * written in the form of:
   * <tt>comm:COM2;baudrate=9600;bitsperchar=8;parity=none;stopbits=1</tt>.
   */
  public static final String SCHEME = "comm";

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
      final RXTXPort port = CommPortUtils.getSerialPort( options.getPortName() );
      port.setSerialPortParams( options.getBaudrate(), options.getDatabits(), options.getStopbits(),
          options.getParityMode() );
      port.setFlowControlMode( options.getFlowControl() );

      port.enableReceiveTimeout( 250 );

      return new SerialConnection( port );
    }
    catch ( PortInUseException exception )
    {
      throw new IOException( "Cannot access port! Port in use?" );
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
}
