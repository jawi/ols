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
import java.util.regex.*;

import nl.lxtreme.ols.util.*;


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

    return portList.toArray( new String[portList.size()] );
  }

  /**
   * Enumerates all devices below the given base path.
   * 
   * @param aDeviceBasePath
   *          the base path of the devices to enumerate, cannot be
   *          <code>null</code>.
   * @return a colon-separated string with all found devices, never
   *         <code>null</code>.
   */
  static final String enumerateDevices( final String aDeviceBasePath )
  {
    final StringBuilder result = new StringBuilder();

    final String deviceRegEx;
    if ( HostUtils.isUnix() || HostUtils.isMacOSX() )
    {
      if ( HostUtils.isUnix() )
      {
        deviceRegEx = "tty\\w+\\d+";
      }
      else
      {
        deviceRegEx = "tty\\..+";
      }

      final Pattern pattern = Pattern.compile( deviceRegEx );

      final File basePath = new File( aDeviceBasePath );
      for ( String fileName : basePath.list() )
      {
        final Matcher matcher = pattern.matcher( fileName );
        if ( matcher.matches() )
        {
          if ( result.length() > 0 )
          {
            result.append( ":" );
          }
          final File device = new File( basePath, fileName );
          result.append( device.getAbsolutePath() );
        }
      }
    }
    else
    {
      throw new RuntimeException( "Unsupported operating system!" );
    }

    return result.toString();
  }
}
