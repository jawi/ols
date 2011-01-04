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
import java.util.regex.*;


/**
 * Provides some common utilities for talking to (serial/parallel) communication
 * ports.
 */
public final class CommPortUtils
{
  // VARIABLES

  private static boolean initialized = false;
  private static String osName = "";

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

    if ( !initialized )
    {
      initialize();
      System.out.println( RXTXVersion.getVersion() + " initialized..." );
      initialized = true;
    }

    RXTXPort port = null;
    int tries = 3;

    if ( !specialFileExists( aPortName ) )
    {
      throw new NoSuchPortException();
    }

    do
    {
      try
      {
        port = internalGetSerialPort( aPortName );
      }
      catch ( NoSuchPortException exception )
      {
        addSerialPort( aPortName );
      }
    }
    while ( ( port == null ) && ( tries >= 0 ) );

    if ( port == null )
    {
      throw new NoSuchPortException();
    }

    return port;
  }

  /**
   * Tries to verify the given port name as a valid device and if this appears
   * so, adds it to the list of devices of RXTX.
   * 
   * @param aPortName
   *          the name of the port to add, cannot be <code>null</code>.
   */
  private static void addSerialPort( final String aPortName ) throws NoSuchPortException
  {
    if ( !osName.contains( "windows" ) )
    {
      File device = new File( aPortName );
      try
      {
        device = device.getCanonicalFile();
        if ( !device.exists() )
        {
          throw new NoSuchPortException();
        }
      }
      catch ( IOException exception )
      {
        throw new NoSuchPortException();
      }
    }
    else
    {
      final Pattern pattern = Pattern.compile( "^COM\\d+$", Pattern.CASE_INSENSITIVE );
      final Matcher matcher = pattern.matcher( aPortName );
      if ( !matcher.matches() )
      {
        throw new NoSuchPortException();
      }
    }

    // Try to add the portname as possible serial port...
    CommPortIdentifier.addPortName( aPortName, CommPortIdentifier.PORT_SERIAL, createDriver() );
  }

  /**
   * Creates a new CommDriver instance.
   * 
   * @return a CommDriver instance, fully initialized, never <code>null</code>.
   */
  private static CommDriver createDriver()
  {
    final RXTXCommDriver driver = new RXTXCommDriver();
    driver.initialize();
    return driver;
  }

  /**
   * Initializes this RXTX library with some platform-specific hacks.
   */
  private static void initialize()
  {
    // Suppress the version mismatch banner...
    System.setProperty( "gnu.io.rxtx.NoVersionOutput", "true" );

    osName = System.getProperty( "os.name" );
    if ( osName == null )
    {
      osName = "unknown";
    }
    osName = osName.toLowerCase();

    // Ugly hack: force a greater set of port names for some OSs...
    if ( osName.contains( "linux" ) )
    {
      // Set taken from RXTXCommDriver.java ...
      final String[] portPrefixes = { "/dev/ttyS", "/dev/ttySA", "/dev/ttyACM", "/dev/ttyUSB", "/dev/rfcomm",
          "/dev/ttyircomm" };
      final StringBuilder sb = new StringBuilder();
      for ( String portPrefix : portPrefixes )
      {
        for ( int i = 0; i < 8; i++ )
        {
          if ( sb.length() > 0 )
          {
            sb.append( ":" );
          }
          sb.append( portPrefix + i );
        }
      }
      System.setProperty( "gnu.io.rxtx.SerialPorts", sb.toString() );
    }
  }

  /**
   * @param aPortName
   * @return
   * @throws NoSuchPortException
   * @throws PortInUseException
   */
  private static RXTXPort internalGetSerialPort( final String aPortName ) throws NoSuchPortException,
      PortInUseException
  {
    final CommPortIdentifier portId = CommPortIdentifier.getPortIdentifier( aPortName );
    return ( RXTXPort )portId.open( "RxTx client library", 1000 );
  }

  /**
   * @param aPath
   * @return
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
