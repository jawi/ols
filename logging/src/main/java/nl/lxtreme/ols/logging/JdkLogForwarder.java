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
package nl.lxtreme.ols.logging;


import java.io.*;
import java.text.*;
import java.util.*;
import java.util.logging.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;
import org.osgi.util.tracker.*;


/**
 * Forwards all JDK logging statements to the OSGi LogService.
 * <p>
 * Based on <tt>Logging OSGi Applications - The Simple and Robust way</tt> by
 * Valery Abu-Eid.
 * 
 * @see http 
 *      ://www.dynamicjava.org/articles/osgi-matters/logging-osgi-the-simple-way
 */
public final class JdkLogForwarder
{
  // INNER TYPES

  /**
   * Provides a bundle tracker to updating the loggers of all additionally
   * loaded classes.
   */
  final class BundleTracker implements BundleListener
  {
    // METHODS

    /**
     * @see org.osgi.framework.BundleListener#bundleChanged(org.osgi.framework.BundleEvent)
     */
    @Override
    public void bundleChanged( final BundleEvent aEvent )
    {
      if ( aEvent.getType() == BundleEvent.INSTALLED )
      {
        updateLogHandler( getCurrentHandler() );
      }
    }
  }

  /**
   * 
   */
  final class LogServiceServiceTracker extends ServiceTracker
  {
    // CONSTRUCTORS

    /**
     * Creates a new LogServiceServiceTracker instance.
     * 
     * @param aBundleContext
     *          the current bundle context to use.
     */
    public LogServiceServiceTracker( final BundleContext aBundleContext )
    {
      super( aBundleContext, LOG_SERVICE_CLASS_NAME, null /* aServiceCustomizer */);
    }

    /**
     * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
     */
    @Override
    public Object addingService( final ServiceReference aReference )
    {
      final LogService logService = ( LogService )super.addingService( aReference );
      setCurrentHandler( logService );
      return logService;
    }

    /**
     * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
     *      java.lang.Object)
     */
    @Override
    public void removedService( final ServiceReference aReference, final Object aService )
    {
      setCurrentHandler( null );
      super.removedService( aReference, aService );
    }
  }

  /**
   * Provides a JDK-Handler that delegates all log records to the OSGi logging
   * service.
   */
  static class OsgiLogDelegateHandler extends Handler
  {
    // VARIABLES

    private final LogService logService;

    // CONSTRUCTORS

    /**
     * Creates a new OsgiLogDelegateHandler instance.
     * 
     * @param aLogService
     *          the OSGi log service to use, cannot be <code>null</code>.
     */
    public OsgiLogDelegateHandler( final LogService aLogService )
    {
      this.logService = aLogService;
    }

    // METHODS

    /**
     * Converts a JDK log level to an accompanying level from the OSGi
     * LogService.
     * 
     * @param aLevel
     *          the JDK log level to convert.
     * @return a OSGi LogService level corresponding to the given level.
     */
    private static final int toOSGiLogLevel( final Level aLevel )
    {
      final boolean all = ( aLevel == Level.ALL );
      if ( all || ( aLevel == Level.SEVERE ) )
      {
        return LogService.LOG_ERROR;
      }
      else if ( all || ( aLevel == Level.WARNING ) )
      {
        return LogService.LOG_WARNING;
      }
      else if ( all || ( aLevel == Level.INFO ) || ( aLevel == Level.CONFIG ) )
      {
        return LogService.LOG_INFO;
      }

      return LogService.LOG_DEBUG;
    }

    /**
     * @see java.util.logging.Handler#close()
     */
    @Override
    public void close() throws SecurityException
    {
      // NO-op
    }

    /**
     * @see java.util.logging.Handler#flush()
     */
    @Override
    public void flush()
    {
      // NO-op
    }

    /**
     * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
     */
    @Override
    public void publish( final LogRecord aRecord )
    {
      if ( aRecord.getLevel() == Level.OFF )
      {
        return;
      }

      if ( aRecord.getThrown() != null )
      {
        this.logService.log( toOSGiLogLevel( aRecord.getLevel() ), getMessage( aRecord ), aRecord.getThrown() );
      }
      else
      {
        this.logService.log( toOSGiLogLevel( aRecord.getLevel() ), getMessage( aRecord ) );
      }
    }

    /**
     * @param aRecord
     * @return
     */
    private String getMessage( final LogRecord aRecord )
    {
      String result = "[[".concat( aRecord.getLoggerName() ).concat( "]]" ).concat( aRecord.getMessage() );

      final Object[] params = aRecord.getParameters();
      if ( ( params != null ) && ( params.length > 0 ) )
      {
        return MessageFormat.format( result, params );
      }
      return result;
    }
  }

  // CONSTANTS

  private static final String LOG_SERVICE_CLASS_NAME = LogService.class.getName();

  // VARIABLES

  private final BundleContext bundleContext;
  private final Handler defaultHandler;
  private final ServiceTracker logServiceTracker;
  private final BundleListener bundleListener;

  private Handler logHandler;

  // CONSTRUCTORS

  /**
   * @param aBundleContext
   * @param aLoggerNames
   */
  public JdkLogForwarder( final BundleContext aBundleContext )
  {
    this( aBundleContext, null );
  }

  /**
   * @param aBundleContext
   * @param aLoggerNames
   * @param aDefaultHandler
   */
  public JdkLogForwarder( final BundleContext aBundleContext, final Handler aDefaultHandler )
  {
    this.bundleContext = aBundleContext;
    this.defaultHandler = ( aDefaultHandler != null ) ? aDefaultHandler : new ConsoleHandler();

    this.logServiceTracker = new LogServiceServiceTracker( aBundleContext );
    this.bundleListener = new BundleTracker();

    this.logHandler = this.defaultHandler;
  }

  // METHODS

  /**
   * Starts the JDK-logging forwarding service.
   */
  public void start() throws IOException
  {
    this.bundleContext.addBundleListener( this.bundleListener );

    this.logServiceTracker.open();
  }

  /**
   * Stops the JDK-logging forwarding service.
   */
  public void stop()
  {
    this.bundleContext.removeBundleListener( this.bundleListener );

    this.logServiceTracker.close();

    final LogManager logManager = LogManager.getLogManager();
    logManager.reset();
  }

  /**
   * Returns the current log handler.
   * 
   * @return a log handler, never <code>null</code>.
   */
  final Handler getCurrentHandler()
  {
    return this.logHandler;
  }

  /**
   * Wraps the given log service as the current log handler. When
   * <code>null</code> is given, the default handler will be set.
   * 
   * @param aLogService
   *          the log service to set, can be <code>null</code>.
   */
  final void setCurrentHandler( final LogService aLogService )
  {
    if ( aLogService != null )
    {
      this.logHandler = new OsgiLogDelegateHandler( aLogService );
      updateLogHandler( this.logHandler );
    }
    else
    {
      this.logHandler = this.defaultHandler;
      updateLogHandler( this.logHandler );
    }
  }

  /**
   * Updates all current loggers to use the given log service...
   */
  final void updateLogHandler( final Handler aHandler )
  {
    final LogManager logManager = LogManager.getLogManager();

    final Enumeration<String> loggerNames = logManager.getLoggerNames();
    while ( loggerNames.hasMoreElements() )
    {
      final String loggerName = loggerNames.nextElement();

      final Logger logger = logManager.getLogger( loggerName );
      if ( logger != null )
      {
        if ( isNonJdkLogger( logger ) )
        {
          logger.setUseParentHandlers( false );
          logger.setLevel( Level.ALL );
        }
        if ( !logger.getUseParentHandlers() )
        {
          replaceHandler( logger, aHandler );
        }
      }
    }
    // Replace the global root logger with our own handler...
    replaceHandler( Logger.getLogger( "" ), aHandler );
  }

  /**
   * @param aLoggerName
   * @return
   */
  private boolean isNonJdkLogger( final Logger aLogger )
  {
    final String loggerName = aLogger.getName();
    return !( loggerName.startsWith( "java." ) //
        || loggerName.startsWith( "sun." ) //
        || loggerName.startsWith( "javax." ) //
        || loggerName.equals( Logger.GLOBAL_LOGGER_NAME ) //
    || loggerName.isEmpty() );
  }

  /**
   * Replaces for the given logger <em>all</em> handlers and replaces them with
   * the handler given.
   * 
   * @param aLogger
   *          the logger to replace the handlers for;
   * @param aHandler
   *          the handler to use for the given logger.
   */
  private void replaceHandler( final Logger aLogger, final Handler aHandler )
  {
    final Handler[] handlers = aLogger.getHandlers();
    if ( ( handlers.length == 1 ) && handlers[0].equals( aHandler ) )
    {
      return;
    }
    for ( Handler handler : handlers )
    {
      aLogger.removeHandler( handler );
      handler.close();
    }
    aLogger.addHandler( aHandler );
  }
}
