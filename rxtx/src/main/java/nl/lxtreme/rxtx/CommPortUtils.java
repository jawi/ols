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


/**
 * Provides some common utilities for talking to (serial/parallel) communication
 * ports.
 */
public final class CommPortUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new CommPortUtils instance. Not used.
   */
  private CommPortUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Returns the names of all currently available (serial) ports.
   * 
   * @return an array containing serial port names, never <code>null</code>.
   */
  @SuppressWarnings( "unchecked" )
  public static String[] getAvailablePorts()
  {
    final Enumeration<CommPortIdentifier> portIdentifiers = CommPortIdentifier.getPortIdentifiers();
    final LinkedList<String> portList = new LinkedList<String>();
    CommPortIdentifier portId = null;

    while ( portIdentifiers.hasMoreElements() )
    {
      portId = portIdentifiers.nextElement();
      if ( portId.getPortType() == CommPortIdentifier.PORT_SERIAL )
      {
        portList.addLast( portId.getName() );
      }
    }

    return ( portList.toArray( new String[portList.size()] ) );
  }

  /**
   * Opens the serial device at the given port name, does nothing more than
   * that.
   * 
   * @param aPortName
   *          the name of the port to open, for example, "/dev/ttyACM0", "com8:"
   *          or any other valid device path.
   * @return the serial port instance, never <code>null</code>.
   * @throws PortInUseException
   *           in case the port could not be opened because it is (exclusively)
   *           locked by another process;
   * @throws NoSuchPortException
   *           if there is no such port with the given name.
   */
  public static RXTXPort getSerialPort( final String aPortName ) throws PortInUseException, NoSuchPortException
  {
    if ( aPortName == null )
    {
      throw new IllegalArgumentException( "Port name cannot be null!" );
    }
    if ( !specialFileExists( aPortName ) )
    {
      throw new NoSuchPortException();
    }

    final RXTXPort port = new RXTXPort( aPortName );
    return port;
  }

  /**
   * Checks whether the path (to a presumed port) exists or not.
   * <p>
   * This method is needed to avoid problems on platforms like Windows where
   * trying to open non-existing ports causes JVM-errors.
   * </p>
   * 
   * @param aPath
   *          the path to the presumed port, cannot be <code>null</code>.
   * @return <code>true</code> if the port exists, <code>false</code> otherwise.
   */
  private static boolean specialFileExists( final String aPath )
  {
    boolean exists;

    final String osName = System.getProperty( "os.name" ).toLowerCase();
    if ( osName.indexOf( "win" ) >= 0 )
    {
      // Windows; use //./comX syntax to denote it's a special file...
      String path = aPath.replaceAll( "\\\\", "/" );
      path = !path.startsWith( "//./" ) ? "//./" + aPath : aPath;

      try
      {
        final FileInputStream fis = new FileInputStream( path );
        try
        {
          fis.close();
        }
        catch ( IOException exception )
        {
          // Ok; we're not allowed to do this, but it is an indicator the file
          // exists...
        }

        exists = true;
      }
      catch ( FileNotFoundException exception )
      {
        // Ok; special file doesn't appear to be existing...
        exists = false;
      }
    }
    else
    {
      final File file = new File( aPath );
      exists = file.exists();
    }

    return exists;
  }

}
