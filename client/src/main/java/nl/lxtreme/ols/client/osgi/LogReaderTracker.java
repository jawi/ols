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
package nl.lxtreme.ols.client.osgi;


import java.io.*;
import java.text.*;
import java.util.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;
import org.osgi.util.tracker.*;


/**
 * @author jawi
 */
public class LogReaderTracker extends ServiceTracker
{
  // INNER TYPES

  /**
   * Provides a simple log writer that writes the output to the standard
   * console.
   */
  static final class SimpleLogWriter implements LogListener
  {
    // CONSTANTS

    private static final int LOGGER_NAME_WIDTH = 35;

    // VARIABLES

    private final DateFormat formatter;
    private final PrintStream out;

    // CONSTRUCTORS

    /**
     * Creates a new SimpleLogWriter instance.
     */
    public SimpleLogWriter()
    {
      this( System.out );
    }

    /**
     * Creates a new SimpleLogWriter instance.
     */
    public SimpleLogWriter( final PrintStream aOutputStream )
    {
      this.out = aOutputStream;
      this.formatter = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.MEDIUM );
    }

    // METHODS

    /**
     * @see org.osgi.service.log.LogListener#logged(org.osgi.service.log.LogEntry)
     */
    @Override
    public void logged( final LogEntry aLogEntry )
    {
      if ( !logEvent( aLogEntry ) )
      {
        return;
      }

      if ( aLogEntry.getMessage() != null )
      {
        final StringBuilder sb = new StringBuilder();

        String loggerName = aLogEntry.getBundle().getSymbolicName();
        String msg = aLogEntry.getMessage();
        if ( ( msg != null ) && msg.startsWith( "[[" ) )
        {
          final int idx = msg.indexOf( "]]" );
          loggerName = msg.substring( 2, idx );
          msg = msg.substring( idx + 2 );
        }

        if ( loggerName != null )
        {
          final int length = loggerName.length();
          if ( length > LOGGER_NAME_WIDTH )
          {
            final int offset = length - LOGGER_NAME_WIDTH;
            final int lastDot = Math.min( length, Math.max( offset, loggerName.indexOf( '.', offset ) + 1 ) );
            loggerName = loggerName.substring( lastDot, length );
          }
          loggerName = String.format( "%30s", loggerName );
        }

        sb.append( '[' );
        sb.append( this.formatter.format( new Date( aLogEntry.getTime() ) ) );
        sb.append( " - " );
        sb.append( getLevel( aLogEntry.getLevel() ) );
        sb.append( " - " );
        sb.append( loggerName );
        sb.append( "]: " );
        if ( msg != null )
        {
          sb.append( msg );
        }

        if ( aLogEntry.getException() != null )
        {
          this.out.println( sb.toString() );
          aLogEntry.getException().printStackTrace( this.out );
        }
        else
        {
          this.out.println( sb.toString() );
        }
      }
    }

    /**
     * @param aLogLevel
     * @return
     */
    private String getLevel( final int aLogLevel )
    {
      if ( LogService.LOG_DEBUG == aLogLevel )
      {
        return "DEBUG";
      }
      else if ( LogService.LOG_ERROR == aLogLevel )
      {
        return "ERROR";
      }
      else if ( LogService.LOG_INFO == aLogLevel )
      {
        return "INFO ";
      }
      else if ( LogService.LOG_WARNING == aLogLevel )
      {
        return "WARN ";
      }
      return "     ";
    }

    /**
     * @param aLogEntry
     * @return
     */
    private boolean logEvent( final LogEntry aLogEntry )
    {
      final String msg = aLogEntry.getMessage();
      if ( msg.startsWith( "BundleEvent" ) || msg.startsWith( "FrameworkEvent" ) || msg.startsWith( "ServiceEvent" ) )
      {
        if ( msg.endsWith( "STARTED" ) || msg.endsWith( "RESOLVED" ) || msg.endsWith( "STOPPED" )
            || msg.endsWith( "UNREGISTERING" ) || msg.endsWith( "REGISTERED" ) )
        {
          // return false;
        }
      }
      return true;
    }
  }

  // VARIABLES

  private LogListener logListener;

  // CONSTRUCTORS

  /**
   * @param aContext
   */
  public LogReaderTracker( final BundleContext aContext )
  {
    super( aContext, LogReaderService.class.getName(), null /* aCustomizer */);
  }

  // METHODS

  /**
   * @see org.osgi.util.tracker.ServiceTracker#addingService(org.osgi.framework.ServiceReference)
   */
  @Override
  public Object addingService( final ServiceReference aReference )
  {
    final LogReaderService logReaderService = ( LogReaderService )super.addingService( aReference );
    if ( this.logListener == null )
    {
      this.logListener = new SimpleLogWriter();
    }
    logReaderService.addLogListener( this.logListener );
    return logReaderService;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#close()
   */
  @Override
  public void close()
  {
    super.close();
    this.logListener = null;
  }

  /**
   * @see org.osgi.util.tracker.ServiceTracker#removedService(org.osgi.framework.ServiceReference,
   *      java.lang.Object)
   */
  @Override
  public void removedService( final ServiceReference aReference, final Object aService )
  {
    if ( this.logListener != null )
    {
      ( ( LogReaderService )aService ).removeLogListener( this.logListener );
    }
    super.removedService( aReference, aService );
  }
}
