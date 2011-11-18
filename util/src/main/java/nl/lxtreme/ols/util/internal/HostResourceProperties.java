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
package nl.lxtreme.ols.util.internal;


import java.io.*;
import java.net.*;
import java.util.*;

import nl.lxtreme.ols.util.*;

import org.osgi.framework.*;


/**
 * Provides a class that holds some client properties, such as the version and
 * so on.
 */
public class HostResourceProperties implements HostProperties
{
  // CONSTANTS

  private static final String HOST_PROPERTIES = "/host.properties";

  private static final String SHORT_NAME = "LogicSniffer";
  private static final String FULL_NAME = SHORT_NAME.concat( " - Logic Analyzer Client" );

  // VARIABLES

  private final Properties properties;

  // CONSTRUCTORS

  /**
   * Creates a new Version instance.
   * 
   * @param aContext
   *          the bundle context to use.
   */
  HostResourceProperties( final BundleContext aContext )
  {
    this.properties = new Properties();

    // Try to load the embedded properties...
    URL resource = aContext.getBundle().getResource( HOST_PROPERTIES );
    if ( resource != null )
    {
      InputStream is = null;

      try
      {
        is = resource.openStream();

        this.properties.load( is );
      }
      catch ( IOException exception )
      {
        // TODO handle exception!
      }
      finally
      {
        HostUtils.closeResource( is );
        resource = null;
      }

      final String osName = aContext.getProperty( Constants.FRAMEWORK_OS_NAME );
      this.properties.put( Constants.FRAMEWORK_OS_NAME, osName );

      final String osVersion = aContext.getProperty( Constants.FRAMEWORK_OS_VERSION );
      this.properties.put( Constants.FRAMEWORK_OS_VERSION, osVersion );

      final String processor = aContext.getProperty( Constants.FRAMEWORK_PROCESSOR );
      this.properties.put( Constants.FRAMEWORK_PROCESSOR, processor );

      final String javaVersion = aContext.getProperty( Constants.FRAMEWORK_EXECUTIONENVIRONMENT );
      this.properties.put( Constants.FRAMEWORK_EXECUTIONENVIRONMENT, javaVersion );
    }
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String getExecutionEnvironment()
  {
    return System.getProperty( "java.vendor" ) + ", v" + System.getProperty( "java.version" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getFullName()
  {
    return FULL_NAME;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getOSName()
  {
    return this.properties.getProperty( Constants.FRAMEWORK_OS_NAME );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getOSVersion()
  {
    return this.properties.getProperty( Constants.FRAMEWORK_OS_VERSION );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getProcessor()
  {
    return this.properties.getProperty( Constants.FRAMEWORK_PROCESSOR );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getReportIncidentAddress()
  {
    return this.properties.getProperty( "client.incidentAddress", "info+ols@lxtreme.nl" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getShortName()
  {
    return SHORT_NAME;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getVersion()
  {
    return this.properties.getProperty( "client.version", "<NO VERSION>" );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDebugMode()
  {
    return Boolean.parseBoolean( System.getProperty( "nl.lxtreme.ols.client.debug", "false" ) );
  }
}
