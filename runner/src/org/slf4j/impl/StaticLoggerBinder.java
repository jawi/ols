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
package org.slf4j.impl;


import org.osgi.service.log.*;
import org.slf4j.*;
import org.slf4j.helpers.*;


/**
 * A bridge for briding SLF4J API-calls to OSGi {@link LogService} calls.
 * <p>
 * Implementation note(s): this class <b>must</b> be named "StaticLoggerBinder",
 * be placed in the "org.slf4j.impl" package and have a public static
 * "getSingleton" method in order for SLF4J to pick it up and use it.<br>
 * </p>
 * 
 * @see LoggerFactory
 */
public class StaticLoggerBinder extends MarkerIgnoringBase implements ILoggerFactory, Logger
{
  // CONSTANTS

  /**
   * Used by SLF4J to check the compatibility of this logger with its own API.
   */
  public static final String REQUESTED_API_VERSION = "1.6";

  private static final long serialVersionUID = 1L;
  private static final StaticLoggerBinder INSTANCE = new StaticLoggerBinder();

  // VARIABLES

  private volatile int logLevel;
  private volatile LogService logService;

  // CONSTRUCTORS

  /**
   * Creates a new {@link StaticLoggerBinder} instance.
   */
  public StaticLoggerBinder()
  {
  }

  // METHODS

  /**
   * @return the instance of this {@link StaticLoggerBinder}, never
   *         <code>null</code>.
   */
  public static StaticLoggerBinder getSingleton()
  {
    return INSTANCE;
  }

  /**
   * @param aLogService
   *          the log service to bridge;
   * @param aLogLevel
   *          the log level to use.
   */
  public void bridgeTo( LogService aLogService, int aLogLevel )
  {
    this.logService = aLogService;
    this.logLevel = aLogLevel;
  }

  @Override
  public void debug( String msg )
  {
    log( LogService.LOG_DEBUG, msg, null );
  }

  @Override
  public void debug( String msg, Object arg1 )
  {
    debug( MessageFormatter.format( msg, arg1 ).toString() );
  }

  @Override
  public void debug( String msg, Object arg1, Object arg2 )
  {
    debug( MessageFormatter.format( msg, arg1, arg2 ).toString() );
  }

  @Override
  public void debug( String msg, Object[] arg1 )
  {
    debug( MessageFormatter.arrayFormat( msg, arg1 ).toString() );
  }

  @Override
  public void debug( String msg, Throwable ex )
  {
    log( LogService.LOG_DEBUG, msg, ex );
  }

  @Override
  public void error( String msg )
  {
    log( LogService.LOG_ERROR, msg, null );
  }

  @Override
  public void error( String msg, Object arg1 )
  {
    error( MessageFormatter.format( msg, arg1 ).toString() );
  }

  @Override
  public void error( String msg, Object arg1, Object arg2 )
  {
    error( MessageFormatter.format( msg, arg1, arg2 ).toString() );
  }

  @Override
  public void error( String msg, Object[] arg1 )
  {
    error( MessageFormatter.arrayFormat( msg, arg1 ).toString() );
  }

  @Override
  public void error( String msg, Throwable ex )
  {
    log( LogService.LOG_ERROR, msg, ex );
  }

  @Override
  public Logger getLogger( String name )
  {
    return this;
  }

  public ILoggerFactory getLoggerFactory()
  {
    return this;
  }

  public String getLoggerFactoryClassStr()
  {
    return getClass().getName();
  }

  @Override
  public String getName()
  {
    return "OSGi";
  }

  @Override
  public void info( String msg )
  {
    log( LogService.LOG_INFO, msg, null );
  }

  @Override
  public void info( String msg, Object arg1 )
  {
    info( MessageFormatter.format( msg, arg1 ).toString() );
  }

  @Override
  public void info( String msg, Object arg1, Object arg2 )
  {
    info( MessageFormatter.format( msg, arg1, arg2 ).toString() );
  }

  @Override
  public void info( String msg, Object[] arg1 )
  {
    info( MessageFormatter.arrayFormat( msg, arg1 ).toString() );
  }

  @Override
  public void info( String msg, Throwable ex )
  {
    log( LogService.LOG_INFO, msg, ex );
  }

  @Override
  public boolean isDebugEnabled()
  {
    return this.logLevel >= 5;
  }

  @Override
  public boolean isDebugEnabled( Marker m )
  {
    return isDebugEnabled();
  }

  @Override
  public boolean isErrorEnabled()
  {
    return true;
  }

  @Override
  public boolean isErrorEnabled( Marker m )
  {
    return true;
  }

  @Override
  public boolean isInfoEnabled()
  {
    return true;
  }

  @Override
  public boolean isInfoEnabled( Marker m )
  {
    return true;
  }

  @Override
  public boolean isTraceEnabled()
  {
    return this.logLevel > 5;
  }

  @Override
  public boolean isTraceEnabled( Marker m )
  {
    return isTraceEnabled();
  }

  @Override
  public boolean isWarnEnabled()
  {
    return true;
  }

  @Override
  public boolean isWarnEnabled( Marker m )
  {
    return true;
  }

  @Override
  public void trace( String msg )
  {
    if ( this.logLevel > 5 )
    {
      debug( "[TRACE] " + msg );
    }
  }

  @Override
  public void trace( String msg, Object arg1 )
  {
    if ( this.logLevel > 5 )
    {
      debug( MessageFormatter.format( "[TRACE] " + msg, arg1 ).toString() );
    }
  }

  @Override
  public void trace( String msg, Object arg1, Object arg2 )
  {
    if ( this.logLevel > 5 )
    {
      debug( MessageFormatter.format( "[TRACE] " + msg, arg1, arg2 ).toString() );
    }
  }

  @Override
  public void trace( String msg, Object[] arg1 )
  {
    if ( this.logLevel > 5 )
    {
      debug( MessageFormatter.arrayFormat( "[TRACE] " + msg, arg1 ).toString() );
    }
  }

  @Override
  public void trace( String msg, Throwable ex )
  {
    if ( this.logLevel > 5 )
    {
      debug( "[TRACE] " + msg, ex );
    }
  }

  @Override
  public void warn( String msg )
  {
    log( LogService.LOG_WARNING, msg, null );
  }

  @Override
  public void warn( String msg, Object arg1 )
  {
    warn( MessageFormatter.format( msg, arg1 ).toString() );
  }

  @Override
  public void warn( String msg, Object arg1, Object arg2 )
  {
    warn( MessageFormatter.format( msg, arg1, arg2 ).toString() );
  }

  @Override
  public void warn( String msg, Object[] arg1 )
  {
    warn( MessageFormatter.arrayFormat( msg, arg1 ).toString() );
  }

  @Override
  public void warn( String msg, Throwable ex )
  {
    log( LogService.LOG_WARNING, msg, ex );
  }

  private void log( int level, String message, Throwable exception )
  {
    try
    {
      this.logService.log( level, message, exception );
    }
    catch ( NullPointerException e )
    {
      // we can safely ignore these
    }
  }
}
