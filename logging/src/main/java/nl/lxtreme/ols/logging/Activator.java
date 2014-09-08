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


import java.util.logging.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.log.*;


/**
 * Bundle activator.
 */
public class Activator extends DependencyActivatorBase
{
  // CONSTANTS

  private static final String PROPERTY_LOG_LEVEL = "nl.lxtreme.ols.logLevel";
  private static final String PROPERTY_LOG_TO_CONSOLE = "nl.lxtreme.ols.logToConsole";
  private static final String PROPERTY_FILTER_JDKUI_LOGS = "nl.lxtreme.ols.filterJdkUiLogs";

  // METHODS

  /**
   * Returns the default log level to use in JUL.
   * 
   * @return a log level, never <code>null</code>.
   */
  public static Level getJavaLogLevel()
  {
    int level = getOsgiLogLevel();

    switch ( level )
    {
      case 0:
        return Level.OFF;
      case LogService.LOG_ERROR:
        return Level.SEVERE;
      case LogService.LOG_WARNING:
        return Level.WARNING;
      case LogService.LOG_INFO:
        return Level.INFO;
      case LogService.LOG_DEBUG:
        return Level.FINE;
      case 5:
        return Level.FINER;
      default:
        return Level.FINEST;
    }
  }

  /**
   * Returns the default log level to use in OSGi LogService.
   * 
   * @return a log level, never <code>null</code>.
   */
  public static int getOsgiLogLevel()
  {
    int level = 3; // = INFO
    try
    {
      level = Integer.getInteger( PROPERTY_LOG_LEVEL, level ).intValue();
    }
    catch ( NumberFormatException exception )
    {
      // Ignore...
    }
    return level;
  }

  /**
   * Returns whether or not we're running in debug mode.
   * 
   * @return <code>true</code> if debug mode is enabled, <code>false</code>
   *         otherwise.
   */
  public static boolean isDebugMode()
  {
    return Boolean.parseBoolean( System.getProperty( PROPERTY_LOG_TO_CONSOLE, "false" ) );
  }

  /**
   * Returns whether or not we should filter out the UI-logs from the JDK.
   * 
   * @return <code>true</code> if UI-logs should be filtered, <code>false</code>
   *         otherwise.
   */
  public static boolean isFilterJdkUILogs()
  {
    return Boolean.parseBoolean( System.getProperty( PROPERTY_FILTER_JDKUI_LOGS, "true" ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy( BundleContext aContext, DependencyManager aManager ) throws Exception
  {
    // Nop
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init( BundleContext aContext, DependencyManager aManager ) throws Exception
  {
    aManager.add( createComponent() //
        .setInterface( LogService.class.getName(), null ) //
        .setImplementation( ConsoleLogger.class ) //
        );

    aManager.add( createComponent() //
        .setImplementation( LogHandler.class ) //
        .add( createServiceDependency() //
            .setService( LogService.class ) //
            .setRequired( false ) ) //
        );
  }
}
