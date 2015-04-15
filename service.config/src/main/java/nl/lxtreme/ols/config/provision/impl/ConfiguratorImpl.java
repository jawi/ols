/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright (c) 2010-2014 The Amdatu Foundation
 */
package nl.lxtreme.ols.config.provision.impl;


import static org.osgi.service.log.LogService.*;

import java.io.*;
import java.util.*;

import nl.lxtreme.ols.config.provision.*;

import org.apache.felix.dm.*;
import org.osgi.framework.*;
import org.osgi.service.cm.*;
import org.osgi.service.log.*;


/**
 * Reads Java properties files and provisions them to ConfigurationAdmin.
 * <p>
 * This code is taken and modified from Amdatu-Configurator.
 * </p>
 */
public class ConfiguratorImpl implements Configurator
{
  // CONSTANTS

  public static final String KEY_CONFIG_DIR = "nl.lxtreme.ols.config.dir";

  // VARIABLES

  // Injected by Felix DM...
  private volatile ConfigurationAdmin configAdmin;
  private volatile LogService log;

  private final ConfigFileRepository repository;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ConfiguratorImpl} instance.
   */
  public ConfiguratorImpl()
  {
    this.repository = new ConfigFileRepository();
  }

  // METHODS

  static void closeQuietly( final Closeable aResource )
  {
    try
    {
      if ( aResource != null )
      {
        aResource.close();
      }
    }
    catch ( IOException exception )
    {
      // Ignore, assume it is not a big deal...
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean provision( final File file ) throws IOException
  {
    this.log.log( LOG_INFO, "Installing configuration from \"" + file.getName() + "\"..." );

    ConfigFileResource resource = new ConfigFileResource( file );

    this.log.log( LOG_INFO, "Provisioning configuration \"" + resource.getId() + "\"..." );

    return provision( resource );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void provisionAll( final File dir ) throws IOException
  {
    Exception firstException = null;

    File[] files = listConfigurations( dir );
    for ( File file : files )
    {
      try
      {
        provision( file );
      }
      catch ( Exception exception )
      {
        if ( firstException == null )
        {
          firstException = exception;
        }
        this.log.log( LOG_WARNING, "Failed to provision \"" + file.getName() + "\" as configuration...", exception );
      }
    }

    if ( firstException instanceof RuntimeException )
    {
      throw ( RuntimeException )firstException;
    }
    else if ( firstException instanceof IOException )
    {
      throw ( IOException )firstException;
    }
    else if ( firstException != null )
    {
      throw new RuntimeException( "Failed to provision configuration!", firstException );
    }
  }

  /**
   * Called by Felix DM when starting this component.
   */
  protected void start( final Component component ) throws IOException
  {
    BundleContext context = component.getDependencyManager().getBundleContext();

    File configDir = getConfigDirectory( context );
    File repositoryFile = getRepositoryFile( context );
    // Load the earlier provisioned configurations...
    this.repository.load( repositoryFile );

    try
    {
      provisionAll( configDir );
    }
    finally
    {
      // Store the earlier provisioned configurations...
      this.repository.store( repositoryFile );
    }
  }

  /**
   * Called by Felix DM when stopping this component.
   */
  protected void stop( final Component component ) throws IOException
  {
    BundleContext context = component.getDependencyManager().getBundleContext();

    File repositoryFile = getRepositoryFile( context );
    // Store the earlier provisioned configurations (just to be sure)...
    this.repository.store( repositoryFile );
  }

  protected void setConfigurationAdmin( final ConfigurationAdmin configAdmin )
  {
    this.configAdmin = configAdmin;
  }

  protected void setLogService( final LogService log )
  {
    this.log = log;
  }

  private File getConfigDirectory( final BundleContext context ) throws IOException
  {
    String dir = context.getProperty( KEY_CONFIG_DIR );
    if ( dir == null )
    {
      throw new IOException( "No configuration directory set! Use property " + KEY_CONFIG_DIR + " for that!" );
    }
    return new File( dir );
  }

  private File getRepositoryFile( final BundleContext context ) throws IOException
  {
    File dataArea = context.getDataFile( "" );
    if ( dataArea == null )
    {
      throw new IOException( "No peristent storage supported..." );
    }
    return new File( dataArea, "configs.repo" );
  }

  private File[] listConfigurations( final File dir ) throws IOException
  {
    if ( !dir.exists() )
    {
      this.log.log( LOG_WARNING, "Configuration directory \"" + dir + "\" does not exist!" );
      return new File[0];
    }
    return dir.listFiles( new FilenameFilter()
    {
      @Override
      public boolean accept( final File dir, final String name )
      {
        return name.endsWith( ".cfg" );
      }
    } );
  }

  private boolean provision( final ConfigFileResource resource ) throws IOException
  {
    Dictionary<String, ?> props = resource.getConfiguration();

    String pid = resource.getPid();
    String factoryPID = resource.getFactoryPid();

    Configuration configuration;
    if ( resource.isFactoryConfig() )
    {
      String generatedPid = this.repository.getConfigurationPid( resource );
      if ( generatedPid == null )
      {
        this.log.log( LOG_DEBUG, "Creating new configuration for \"" + factoryPID + "\" (not found in repository)..." );
        // See OSGi compendium r4.2.0, section 114.4.1...
        configuration = this.configAdmin.createFactoryConfiguration( factoryPID, null );
      }
      else
      {
        this.log.log( LOG_DEBUG, "Obtaining configuration for \"" + generatedPid + "\" (found in repository)..." );
        // See OSGi compendium r4.2.0, section 114.4.1...
        configuration = this.configAdmin.getConfiguration( generatedPid, null );
      }
    }
    else
    {
      this.log.log( LOG_DEBUG, "Obtaining configuration for \"" + pid + "\" (maybe found in repository)..." );
      // See OSGi compendium r4.2.0, section 114.4.1...
      configuration = this.configAdmin.getConfiguration( pid, null );
    }

    if ( configuration != null )
    {
      if ( this.repository.add( configuration, resource ) )
      {
        this.log.log( LOG_INFO, "Provisioned new configuration: " + resource.getId() );

        configuration.update( props );

        return true;
      }
    }

    return false;
  }
}
