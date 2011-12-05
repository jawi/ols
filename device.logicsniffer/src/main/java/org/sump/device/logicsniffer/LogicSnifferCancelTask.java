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
import java.util.*;
import java.util.logging.*;

import javax.microedition.io.*;

import nl.lxtreme.ols.api.devices.*;
import nl.lxtreme.ols.util.*;


/**
 * Cancels the ongoing acquisition.
 */
public class LogicSnifferCancelTask implements CancelTask
{
  // CONSTANTS

  /** reset analyzer */
  private static final byte CMD_RESET = 0x00;
  /** ask the device to immediately return its RLE-encoded data. */
  private static final byte CMD_RLE_FINISH_NOW = 0x05;

  private static final Logger LOG = Logger.getLogger( LogicSnifferCancelTask.class.getName() );

  // VARIABLES

  private final LogicSnifferConfig config;
  private final StreamConnection connection;

  // CONSTRUCTORS

  /**
   * Creates a new LogicSnifferCancelTask instance.
   * 
   * @param aConnection
   * @param aConfig
   */
  public LogicSnifferCancelTask( final LogicSnifferConfig aConfig, final StreamConnection aConnection )
  {
    this.config = aConfig;
    this.connection = aConnection;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Void call() throws Exception
  {
    final OutputStream outputStream = this.connection.openOutputStream();

    try
    {
      if ( this.config.isRleEnabled() )
      {
        LOG.info( "Prematurely finishing RLE-enabled capture ..." );

        outputStream.write( CMD_RLE_FINISH_NOW );
      }
      else
      {
        LOG.info( "Prematurely finishing normal capture ..." );

        byte[] buffer = new byte[5];
        Arrays.fill( buffer, CMD_RESET );

        outputStream.write( buffer );
      }
      // Make sure nothing keeps lingering in the streams' buffer...
      outputStream.flush();
    }
    catch ( IOException exception )
    {
      if ( !HostUtils.handleInterruptedException( exception ) )
      {
        LOG.log( Level.WARNING, "Stopping capture failed?!", exception );
        throw exception;
      }
    }

    return null;
  }
}
