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
 * Copyright (C) 2010-2013 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.runner;


import java.io.*;
import java.util.*;

import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * An implementation of {@link LogService} that can log to either a console or a
 * log-file.
 */
public class MiniLogger implements LogService
{
  // INNER TYPES

  public static enum LogTarget
  {
    NONE, CONSOLE, FILE;
  }

  // CONSTANTS

  private static String[] LEVEL = { "", "ERROR", "WARN ", "INFO ", "DEBUG" };

  // VARIABLES

  private final PrintStream logTarget;
  private final int logLevel;

  // CONSTRUCTORS

  /**
   * Creates a new {@link MiniLogger} instance.
   */
  public MiniLogger( LogTarget aLogTarget, int aLogLevel )
  {
    try
    {
      this.logTarget = ( aLogTarget == LogTarget.CONSOLE ) ? System.out //
          : ( aLogTarget == LogTarget.FILE ) ? new PrintStream( "ols.log" ) //
              : null;
      this.logLevel = aLogLevel;
    }
    catch ( FileNotFoundException exception )
    {
      throw new RuntimeException( "Failed to open log file!", exception );
    }
  }

  // METHODS

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
    if ( this.logTarget == null || level > this.logLevel )
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
      this.logTarget.println( msg );
    }

    if ( throwable != null )
    {
      throwable.printStackTrace( System.out );
    }
  }

  public void start( BundleContext aContext ) throws Exception
  {
    Dictionary<String, Object> props = new Hashtable<String, Object>();
    props.put( Constants.SERVICE_RANKING, Integer.valueOf( -1 ) );

    aContext.registerService( LogService.class.getName(), this, props );
  }

  public void stop( BundleContext aContext ) throws Exception
  {
    if ( this.logTarget != null && this.logTarget != System.out )
    {
      this.logTarget.close();
    }
  }
}
