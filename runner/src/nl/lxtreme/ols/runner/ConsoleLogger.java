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


import java.lang.reflect.*;
import java.text.*;
import java.util.logging.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * An implementation of the OSGi LogService that directly outputs each log
 * message to <code>System.out</code>. It does not implement the LogReader or
 * LogListeners.
 */
public class ConsoleLogger extends Handler implements LogService
{
  // INNER TYPES

  final class CustomLogManager extends LogManager
  {
    // METHODS

    @Override
    public synchronized boolean addLogger( Logger aLogger )
    {
      boolean result = super.addLogger( aLogger );
      Handler[] handlers = aLogger.getHandlers();
      for ( Handler handler : handlers )
      {
        aLogger.removeHandler( handler );
      }
      aLogger.addHandler( ConsoleLogger.this );
      aLogger.setLevel( Level.FINEST );
      return result;
    }
  }

  // CONSTANTS

  private static final String PROPERTY_FILTER_JDKUI_LOGS = "nl.lxtreme.ols.filterJdkUiLogs";

  private static String[] LEVEL = { "", "ERROR", "WARN ", "INFO ", "DEBUG" };

  // VARIABLES

  private final boolean logToConsole;
  private final int logLevel;

  /**
   * Creates a new ConsoleLogger instance.
   * 
   * @param aLogToConsole
   * @param aLogLevel
   */
  public ConsoleLogger( boolean aLogToConsole, int aLogLevel )
  {
    this.logToConsole = aLogToConsole;
    this.logLevel = aLogLevel;
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

  public void log( int level, String message )
  {
    log( null, level, message, null );
  }

  public void log( int level, String message, Throwable throwable )
  {
    log( null, level, message, throwable );
  }

  @SuppressWarnings( "rawtypes" )
  public void log( ServiceReference reference, int level, String message )
  {
    log( reference, level, message, null );
  }

  @SuppressWarnings( "rawtypes" )
  public void log( ServiceReference reference, int level, String message, Throwable throwable )
  {
    if ( !this.logToConsole || level > this.logLevel )
    {
      return;
    }

    String bundle = " [   ]";
    String service = " ";
    if ( reference != null )
    {
      bundle = String.format( " [%03d] ", Long.valueOf( reference.getBundle().getBundleId() ) );

      Object objectClass = reference.getProperty( Constants.OBJECTCLASS );
      if ( objectClass instanceof String[] )
      {
        StringBuffer buffer = new StringBuffer( "" );
        String[] objClassArr = ( String[] )objectClass;
        for ( int i = 0; i < objClassArr.length; i++ )
        {
          String svc = objClassArr[i];
          if ( buffer.length() > 0 )
          {
            buffer.append( ';' );
          }
          buffer.append( svc );
          service += buffer.toString() + ": ";
        }
      }
      else
      {
        service += objectClass.toString() + ": ";
      }
    }

    String msg = "[" + LEVEL[level] + "]" + bundle + service + message;
    if ( !msg.contains( "org.slf4j.helpers" ) && !message.contains( "TRACE" ) )
    {
      System.out.println( msg );
    }

    if ( throwable != null )
    {
      throwable.printStackTrace( System.out );
    }
  }

  // VARIABLES

  /**
   * {@inheritDoc}
   */
  @Override
  public void publish( final LogRecord aRecord )
  {
    if ( isBannedLogger( aRecord ) || mapLevel( aRecord.getLevel() ) > this.logLevel )
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

    log( mapLevel( aRecord.getLevel() ), message, aRecord.getThrown() );
  }

  /**
   * @param aContext
   * @throws Exception
   */
  final void start( BundleContext aContext ) throws Exception
  {
    CustomLogManager newManager = new CustomLogManager();
    LogManager oldManager;

    Class<LogManager> clazz = LogManager.class;
    synchronized ( clazz )
    {
      // Replace current manager with our own sub-class...
      Field field = clazz.getDeclaredField( "manager" );
      field.setAccessible( true );
      oldManager = ( LogManager )field.get( null );
      field.set( null, newManager );

      // Copy old root logger...
      field = clazz.getDeclaredField( "rootLogger" );
      field.setAccessible( true );
      Logger oldRootLogger = ( Logger )field.get( oldManager );
      field.set( newManager, oldRootLogger );
      newManager.addLogger( oldRootLogger );

      // Cleanup resources of old log manager...
      oldManager.reset();
    }

    aContext.registerService( LogService.class.getName(), this, null );
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
        || name.startsWith( "javax.swing." );
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
