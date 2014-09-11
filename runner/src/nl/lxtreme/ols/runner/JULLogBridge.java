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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.runner;


import java.text.*;
import java.util.logging.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * A bridge for converting JUL {@link LogRecord}s to OSGi {@link LogService}
 * events.
 */
public class JULLogBridge extends Handler
{
  // CONSTANTS

  private static final String PROPERTY_FILTER_JDKUI_LOGS = "nl.lxtreme.ols.filterJdkUiLogs";

  // VARIABLES

  private final LogService logService;

  /**
   * Creates a new {@link JULLogBridge} instance.
   */
  public JULLogBridge( LogService aLogService )
  {
    this.logService = aLogService;
  }

  // METHODS

  /**
   * Returns whether or not we should filter out the UI-logs from the JDK.
   * 
   * @return <code>true</code> if UI-logs should be filtered, <code>false</code>
   *         otherwise.
   */
  private static boolean isFilterJdkUILogs()
  {
    return Boolean.parseBoolean( System.getProperty( PROPERTY_FILTER_JDKUI_LOGS, "true" ) );
  }

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
    if ( isBannedLogger( aRecord ) )
    {
      return;
    }

    Object[] params = aRecord.getParameters();
    String message = aRecord.getMessage();
    if ( ( params != null ) && ( params.length > 0 ) )
    {
      message = MessageFormat.format( message, params );
    }
    String loggerName = aRecord.getLoggerName();
    if ( loggerName.startsWith( "nl.lxtreme.ols" ) )
    {
      // strip the nl.lxtreme...
      loggerName = loggerName.substring( 11 );
    }
    message = String.format( "(%s) %s", loggerName, message );

    this.logService.log( mapLevel( aRecord.getLevel() ), message, aRecord.getThrown() );
  }

  /**
   * Starts this bridge service by replacing the default JUL {@link LogManager}
   * with our own implementation.
   */
  public void start( BundleContext aContext ) throws Exception
  {
    LogManager manager = LogManager.getLogManager();
    manager.reset();
    manager.getLogger( "" ).setLevel( Level.FINE );
    manager.getLogger( "" ).addHandler( this );
  }

  /**
   * Tests whether the given log record comes from a "banned" logger.
   * 
   * @param aRecord
   * @return
   * @see #isFilterJdkUILogs()
   */
  private boolean isBannedLogger( LogRecord aRecord )
  {
    String name = aRecord.getLoggerName();
    if ( name == null || !isFilterJdkUILogs() )
    {
      return false;
    }

    return name.startsWith( "java.awt." ) || name.startsWith( "sun.awt." ) || name.startsWith( "sun.lwawt." )
        || name.startsWith( "javax.swing." ) || name.startsWith( "com.jidesoft." );
  }

  /**
   * Map the log levels of the Java logging API to those of the OSGi LogService.
   */
  private int mapLevel( final Level aLevel )
  {
    if ( aLevel == Level.OFF )
    {
      return 0;
    }
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
}
