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

    return portList.toArray( new String[portList.size()] );
  }
}
