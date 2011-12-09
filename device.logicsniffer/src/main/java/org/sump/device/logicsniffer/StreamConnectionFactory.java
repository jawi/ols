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
package org.sump.device.logicsniffer;


import java.io.*;

import javax.microedition.io.*;

import org.osgi.framework.*;
import org.osgi.service.io.*;
import org.osgi.util.tracker.*;


/**
 * Factory for creating new stream connections.
 */
public class StreamConnectionFactory extends ServiceTracker
{
  // CONSTRUCTORS

  /**
   * Creates a new StreamConnectionFactory instance.
   * 
   * @param aContext
   */
  public StreamConnectionFactory( final BundleContext aContext )
  {
    super( aContext, ConnectorService.class.getName(), null /* aCustomizer */);
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  public StreamConnection getConnection( final String aPortName, final int aPortRate, final boolean aDtrValue,
      final int aOpenDelay ) throws IOException
  {
    final ConnectorService connectorService = ( ConnectorService )getService();
    if ( connectorService == null )
    {
      throw new IOException( "No connector service available!" );
    }

    final String portUri = String.format(
        "comm:%s;baudrate=%d;bitsperchar=8;parity=none;stopbits=1;flowcontrol=xon_xoff;dtr=%s;delay=%d", aPortName,
        Integer.valueOf( aPortRate ), ( aDtrValue ? "on" : "off" ), Integer.valueOf( aOpenDelay ) );

    return ( StreamConnection )connectorService.open( portUri, ConnectorService.READ_WRITE, true /* timeouts */);
  }
}
