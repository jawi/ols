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
package nl.lxtreme.ols.run;


import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * Provides a console logger for the OSGi log service.
 */
public class ConsoleLogger implements LogService
{
  // CONSTANTS

  private static final String[] LEVEL = { "", "Error", "Warn ", "Info ", "Debug" };

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void log( final int aLevel, final String aMessage )
  {
    log( null, aLevel, aMessage, null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void log( final int aLevel, final String aMessage, final Throwable aException )
  {
    log( null, aLevel, aMessage, aException );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void log( final ServiceReference aServiceRef, final int aLevel, final String aMessage )
  {
    log( aServiceRef, aLevel, aMessage, null );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void log( final ServiceReference aServiceRef, final int aLevel, final String aMessage,
      final Throwable aException )
  {
    StringBuilder sb = new StringBuilder();

    sb.append( "[" ).append( LEVEL[aLevel] ).append( "] " );

    if ( aServiceRef != null )
    {
      Long bundleId = Long.valueOf( aServiceRef.getBundle().getBundleId() );
      sb.append( "(" ).append( String.format( "%03d", bundleId ) ).append( ") " );

      Object objectClass = aServiceRef.getProperty( Constants.OBJECTCLASS );
      if ( objectClass instanceof String[] )
      {
        StringBuilder buffer = new StringBuilder();

        String[] objClassArr = ( ( String[] )objectClass );
        for ( String svc : objClassArr )
        {
          if ( buffer.length() > 0 )
          {
            buffer.append( ',' );
          }
          buffer.append( svc );
        }

        sb.append( buffer ).append( ": " );
      }
      else
      {
        sb.append( objectClass ).append( ": " );
      }
    }

    sb.append( aMessage );

    System.out.println( sb.toString() );
    if ( aException != null )
    {
      aException.printStackTrace( System.out );
    }
  }
}
