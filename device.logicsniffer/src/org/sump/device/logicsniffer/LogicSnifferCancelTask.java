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
import java.util.logging.*;

import javax.microedition.io.*;

import org.sump.device.logicsniffer.protocol.*;

import nl.lxtreme.ols.api.devices.*;


/**
 * Cancels the ongoing RLE-enabled acquisition.
 * <p>
 * When using RLE, the acquisition needs to be aborted with a "special" command
 * in order to stop the device from gathering samples. After receiving this
 * command, the device will return its buffer immediately. When using a non-RLE
 * capture, the device can be simply interrupted and we cannot obtain any new
 * samples from it.
 * </p>
 */
public class LogicSnifferCancelTask implements CancelTask, SumpProtocolConstants
{
  // CONSTANTS

  private static final Logger LOG = Logger.getLogger( LogicSnifferCancelTask.class.getName() );

  // VARIABLES

  private final StreamConnection connection;

  // CONSTRUCTORS

  /**
   * Creates a new {@link LogicSnifferCancelTask} instance.
   * 
   * @param aConnection
   *          the stream connection to write the "finish RLE" command to, cannot
   *          be <code>null</code>.
   */
  public LogicSnifferCancelTask( final StreamConnection aConnection )
  {
    this.connection = aConnection;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Void call() throws Exception
  {
    LOG.info( "Prematurely finishing RLE-enabled capture ..." );

    final DataOutputStream outputStream = this.connection.openDataOutputStream();
    // Write a single command to stop RLE from doing its job...
    outputStream.writeByte( CMD_RLE_FINISH_NOW );
    // Make sure nothing keeps lingering in the streams' buffer...
    outputStream.flush();

    return null;
  }
}
