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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.logbridge;


import java.text.*;
import java.util.*;
import java.util.logging.*;

import org.osgi.service.log.*;


/**
 * Implements a custom log handler for forwarding all JUL log statements to the
 * LogService of OSGi.
 */
public class LogHandler extends Handler
{
  // VARIABLES

  // Injected by Felix DM...
  private volatile LogService logService;

  private final List<Handler> originalRootHandlers;

  // CONSTRUCTORS

  /**
   * Creates a new {@link LogHandler} instance.
   */
  public LogHandler()
  {
    this.originalRootHandlers = new ArrayList<Handler>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws SecurityException
  {
    // Nop
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void flush()
  {
    // Nop
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void publish( final LogRecord aRecord )
  {
    if ( aRecord.getLevel() == Level.OFF )
    {
      return;
    }

    Object[] params = aRecord.getParameters();
    String message = aRecord.getMessage();
    if ( ( params != null ) && ( params.length > 0 ) )
    {
      message = MessageFormat.format( message, params );
    }

    this.logService.log( mapLevel( aRecord.getLevel() ), message, aRecord.getThrown() );
  }

  /**
   * Called by Felix DependencyManager when starting this component.
   */
  public synchronized void start() throws Exception
  {
    LogManager logManager = LogManager.getLogManager();

    replaceLogHandlers( logManager.getLogger( "" ), this.originalRootHandlers );
  }

  /**
   * Called by Felix DependencyManager upon stopping this component.
   */
  public synchronized void stop() throws Exception
  {
    LogManager logManager = LogManager.getLogManager();

    restoreLogHandler( logManager.getLogger( "" ), this.originalRootHandlers );
  }

  /**
   * Map the log levels of the Java logging API to those of the OSGi LogService.
   */
  private int mapLevel( final Level aLevel )
  {
    int value = aLevel.intValue();
    if ( value >= Level.SEVERE.intValue() )
    {
      return LogService.LOG_ERROR;
    }
    if ( value >= Level.WARNING.intValue() )
    {
      return LogService.LOG_WARNING;
    }
    if ( value >= Level.INFO.intValue() )
    {
      return LogService.LOG_INFO;
    }
    return LogService.LOG_DEBUG;
  }

  /**
   * @param aLogger
   *          the logger to replace the log handlers for, can be
   *          <code>null</code>;
   * @param aHandlerList
   *          the list of handlers to store the replaced log handlers in, cannot
   *          be <code>null</code>.
   */
  private void replaceLogHandlers( final Logger aLogger, final List<Handler> aHandlerList )
  {
    if ( aLogger == null )
    {
      return;
    }

    Handler[] hs = aLogger.getHandlers();
    for ( Handler h : hs )
    {
      aLogger.removeHandler( h );
      aHandlerList.add( h );
    }

    aLogger.addHandler( this );
  }

  /**
   * @param aLogger
   *          the logger to restore the log handlers for, can be
   *          <code>null</code>;
   * @param aHandlerList
   *          the list of handlers to restore, cannot be <code>null</code>.
   */
  private void restoreLogHandler( final Logger aLogger, final List<Handler> aHandlerList )
  {
    if ( aLogger == null )
    {
      return;
    }

    aLogger.removeHandler( this );

    for ( Handler h : aHandlerList )
    {
      aLogger.addHandler( h );
    }
    aHandlerList.clear();
  }
}
