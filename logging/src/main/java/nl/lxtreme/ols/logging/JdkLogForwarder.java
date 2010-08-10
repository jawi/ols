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
package nl.lxtreme.ols.logging;


import java.util.*;
import java.util.logging.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;


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
   * 
   */
  protected static class DummyLogHandler extends Handler
  {
    /**
     * @see java.util.logging.Handler#close()
     */
    @Override
    public void close() throws SecurityException
    {
    }

    /**
     * @see java.util.logging.Handler#flush()
     */
    @Override
    public void flush()
    {
    }

    /**
     * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
     */
    @Override
    public void publish( final LogRecord aRecord )
    {
    }
  }

  /**
   * 
   */
  protected final class LogServiceServiceListener implements ServiceListener
  {
    /**
     * @see org.osgi.framework.ServiceListener#serviceChanged(org.osgi.framework.ServiceEvent)
     */
    @Override
    public void serviceChanged( final ServiceEvent serviceEvent )
    {
      try
      {
        updateLogHandler();
      }
      catch ( final Throwable ex )
      {
        System.out.println( "Error: " + ex.getMessage() );
        ex.printStackTrace();
      }
    }
  }

  /**
   * 
   */
  protected static class OsgiLogDelegateHandler extends Handler
  {
    private final LogService logService;

    /**
     * @param aLogService
     */
    public OsgiLogDelegateHandler( final LogService aLogService )
    {
      this.logService = aLogService;
    }

    /**
     * @see java.util.logging.Handler#close()
     */
    @Override
    public void close() throws SecurityException
    {
    }

    /**
     * @see java.util.logging.Handler#flush()
     */
    @Override
    public void flush()
    {
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
        getLogService().log( getToOsgiLogLevel( aRecord.getLevel() ), aRecord.getMessage(), aRecord.getThrown() );
      }
      else
      {
        getLogService().log( getToOsgiLogLevel( aRecord.getLevel() ), aRecord.getMessage() );
      }
    }

    /**
     * @return
     */
    protected LogService getLogService()
    {
      return this.logService;
    }

    /**
     * @param aLevel
     * @return
     */
    protected int getToOsgiLogLevel( final Level aLevel )
    {
      if ( aLevel == Level.SEVERE )
      {
        return LogService.LOG_ERROR;
      }
      else if ( aLevel == Level.WARNING )
      {
        return LogService.LOG_WARNING;
      }
      else if ( ( aLevel == Level.INFO ) || ( aLevel == Level.CONFIG ) || ( aLevel == Level.FINE ) )
      {
        return LogService.LOG_INFO;
      }
      else
      {
        return LogService.LOG_DEBUG;
      }
    }
  }

  // CONSTANTS

  protected static final String LOG_SERVICE_CLASS_NAME = LogService.class.getName();

  // VARIABLES

  private final BundleContext bundleContext;
  private final List<String> loggerNames;
  private final Handler defaultHandler;
  private final ServiceListener logServiceServiceListener = new LogServiceServiceListener();
  private ServiceReference lastUsedLogServiceRef;
  private Handler lastUsedHandler;

  // CONSTRUCTORS

  /**
   * @param aBundleContext
   * @param aLoggerNames
   * @param aDefaultHandler
   */
  public JdkLogForwarder( final BundleContext aBundleContext, final Handler aDefaultHandler,
      final String... aLoggerNames )
  {
    this.bundleContext = aBundleContext;
    this.loggerNames = new Vector<String>( Arrays.asList( aLoggerNames ) );
    this.defaultHandler = aDefaultHandler != null ? aDefaultHandler : new DummyLogHandler();
  }

  /**
   * @param aBundleContext
   * @param aLoggerNames
   */
  public JdkLogForwarder( final BundleContext aBundleContext, final String... aLoggerNames )
  {
    this( aBundleContext, null, aLoggerNames );
  }

  // METHODS

  public void addLoggerName( final String aName )
  {
    this.loggerNames.add( aName );
  }

  /**
   * 
   */
  public void start()
  {
    for ( final String loggerName : getLoggerNames() )
    {
      final Logger logger = Logger.getLogger( loggerName );
      logger.setUseParentHandlers( false );
    }
    updateLogHandler();
    addLogServiceListener();
  }

  /**
   * 
   */
  public void stop()
  {
    final BundleContext context = getBundleContext();

    context.removeServiceListener( getLogServiceServiceListener() );
    for ( final String loggerName : getLoggerNames() )
    {
      Logger.getLogger( loggerName ).removeHandler( getLastUsedHandler() );
    }
    if ( getLastUsedLogServiceRef() != null )
    {
      context.ungetService( getLastUsedLogServiceRef() );
    }
  }

  /**
   * 
   */
  protected void addLogServiceListener()
  {
    try
    {
      final BundleContext context = getBundleContext();
      final String filter = String.format( "(%s=%s)", Constants.OBJECTCLASS, LOG_SERVICE_CLASS_NAME );
      context.addServiceListener( getLogServiceServiceListener(), filter );
    }
    catch ( final InvalidSyntaxException ex )
    {
      // / This exception should not occur in the first place
      throw new RuntimeException( ex.getMessage() );
    }
  }

  /**
   * @return
   */
  protected BundleContext getBundleContext()
  {
    return this.bundleContext;
  }

  /**
   * @return
   */
  protected Handler getDefaultHandler()
  {
    return this.defaultHandler;
  }

  /**
   * @return
   */
  protected Handler getLastUsedHandler()
  {
    return this.lastUsedHandler;
  }

  /**
   * @return
   */
  protected ServiceReference getLastUsedLogServiceRef()
  {
    return this.lastUsedLogServiceRef;
  }

  /**
   * @return
   */
  protected List<String> getLoggerNames()
  {
    return this.loggerNames;
  }

  /**
   * @return
   */
  protected ServiceListener getLogServiceServiceListener()
  {
    return this.logServiceServiceListener;
  }

  /**
   * @param aLastUsedHandler
   */
  protected void setLastUsedHandler( final Handler aLastUsedHandler )
  {
    this.lastUsedHandler = aLastUsedHandler;
  }

  /**
   * @param aLastUsedLogServiceRef
   */
  protected void setLastUsedLogServiceRef( final ServiceReference aLastUsedLogServiceRef )
  {
    this.lastUsedLogServiceRef = aLastUsedLogServiceRef;
  }

  /**
   * 
   */
  protected void updateLogHandler()
  {
    final ServiceReference logServiceRef = getBundleContext().getServiceReference( LOG_SERVICE_CLASS_NAME );
    Handler logHandler = null;
    if ( ( logServiceRef != null ) && ( logServiceRef == getLastUsedLogServiceRef() ) )
    {
      return;
    }
    if ( logServiceRef != null )
    {
      logHandler = new OsgiLogDelegateHandler( ( LogService )getBundleContext().getService( logServiceRef ) );
    }
    else
    {
      logHandler = getDefaultHandler();
    }

    updateLoggerHandlers( logHandler );

    if ( getLastUsedLogServiceRef() != null )
    {
      getBundleContext().ungetService( getLastUsedLogServiceRef() );
    }

    setLastUsedLogServiceRef( logServiceRef );
    setLastUsedHandler( logHandler );
  }

  /**
   * @param aLogHandler
   */
  private void updateLoggerHandlers( final Handler aLogHandler )
  {
    final Handler lastUsedHandler = getLastUsedHandler();
    for ( final String loggerName : getLoggerNames() )
    {
      final Logger logger = Logger.getLogger( loggerName );
      logger.removeHandler( lastUsedHandler );
      logger.addHandler( aLogHandler );
    }
  }

}

/* EOF */
