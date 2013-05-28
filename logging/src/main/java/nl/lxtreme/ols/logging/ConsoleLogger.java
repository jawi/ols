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


import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.log.LogService;


/**
 * An implementation of the OSGi LogService that directly outputs each log
 * message to <code>System.out</code>. It does not implement the LogReader or
 * LogListeners.
 */
public class ConsoleLogger implements LogService
{
  private static String[] LEVEL = { "", "ERROR", "WARN ", "INFO ", "DEBUG" };

  /**
   * Returns whether or not we're running in debug mode.
   * 
   * @return <code>true</code> if debug mode is enabled, <code>false</code>
   *         otherwise.
   */
  private static boolean isDebugMode()
  {
    return Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.logToConsole", "false" ) );
  }

  public void log( int level, String message )
  {
    log( null, level, message, null );
  }

  public void log( int level, String message, Throwable throwable )
  {
    log( null, level, message, throwable );
  }

  public void log( ServiceReference reference, int level, String message )
  {
    log( reference, level, message, null );
  }

  public void log( ServiceReference reference, int level, String message, Throwable throwable )
  {
    if ( !isDebugMode() )
    {
      return;
    }

    String bundle = " [   ]";
    String service = " ";
    if ( reference != null )
    {
      bundle = "00" + reference.getBundle().getBundleId();
      bundle = " [" + bundle.substring( bundle.length() - 3 ) + "]";

      Object objectClass = reference.getProperty( Constants.OBJECTCLASS );
      if ( objectClass instanceof String[] )
      {
        StringBuffer buffer = new StringBuffer();
        String[] objClassArr = ( ( String[] )objectClass );
        for ( int i = 0; i < objClassArr.length; i++ )
        {
          String svc = objClassArr[i];
          if ( buffer.length() > 0 )
          {
            buffer.append( ';' );
          }
          buffer.append( svc );
          service = buffer.toString() + ": ";
        }
      }
      else
      {
        service = objectClass.toString() + ": ";
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
}
