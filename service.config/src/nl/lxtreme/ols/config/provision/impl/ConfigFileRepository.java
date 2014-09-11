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


import static nl.lxtreme.ols.config.provision.impl.ConfiguratorImpl.*;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

import org.osgi.service.cm.Configuration;


/**
 * Provides a persistent repository for mapping provisioned configurations to
 * their aliases.
 * <p>
 * This code is taken and modified from Amdatu-Configurator.
 * </p>
 */
public class ConfigFileRepository
{
  // INNER TYPES

  static class ResourceInfo implements Serializable
  {
    private static final long serialVersionUID = 1L;

    final String id;
    final long lastModified;

    public ResourceInfo( final String id, final long lastModified )
    {
      this.id = id;
      this.lastModified = lastModified;
    }
  }

  // VARIABLES

  private final Map<String, ResourceInfo> mapping;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ConfigFileRepository} instance.
   */
  public ConfigFileRepository()
  {
    this.mapping = new HashMap<String, ResourceInfo>();
  }

  // METHODS

  /**
   * Adds a mapping for a provisioned {@link Configuration} to an
   * {@link ConfigFileResource}.
   *
   * @param configuration
   *          the provisioned {@link Configuration} to map, cannot be
   *          <code>null</code>;
   * @param resource
   *          the {@link ConfigFileResource} that maps to the given
   *          configuration, cannot be <code>null</code>.
   * @return <code>true</code> if the mapping was added (did not exist yet),
   *         <code>false</code> otherwise.
   */
  public boolean add( final Configuration configuration, final ConfigFileResource resource )
  {
    String id = createIdentity( resource );
    String pid = configuration.getPid();
    if ( isNewOrChanged( resource ) )
    {
      this.mapping.put( pid, new ResourceInfo( id, resource.getLastModified() ) );
      return true;
    }
    return false;
  }

  /**
   * Returns the PID of the configuration provisioned for the given
   * {@link ConfigFileResource}.
   *
   * @param resource
   *          the {@link ConfigFileResource} to return the configuration PID
   *          for, cannot be <code>null</code>.
   * @return the configuration PID as string, or <code>null</code> if no
   *         configuration was mapped to the given {@link ConfigFileResource}.
   */
  public String getConfigurationPid( final ConfigFileResource resource )
  {
    String id = createIdentity( resource );
    for ( Map.Entry<String, ResourceInfo> entry : this.mapping.entrySet() )
    {
      if ( id.equals( entry.getValue().id ) )
      {
        return entry.getKey();
      }
    }
    return null;
  }

  /**
   * @return <code>true</code> if this repository is empty, <code>false</code>
   *         otherwise.
   */
  public boolean isEmpty()
  {
    return this.mapping.isEmpty();
  }

  /**
   * Loads the contents of this repository from a given {@link File}.
   *
   * @param repoFile
   *          the repository file to load the data from, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading from the repository file.
   */
  @SuppressWarnings( "unchecked" )
  public void load( final File repoFile ) throws IOException
  {
    FileInputStream fis = null;
    ObjectInputStream ois = null;

    try
    {
      fis = new FileInputStream( repoFile );
      ois = new ObjectInputStream( fis );

      this.mapping.putAll( ( Map<String, ResourceInfo> )ois.readObject() );
    }
    catch ( EOFException exception )
    {
      // Ignore, first time start...
    }
    catch ( FileNotFoundException exception )
    {
      // Ignore, first time start...
    }
    catch ( Exception exception )
    {
      throw new IOException( "Invalid repository data?!", exception );
    }
    finally
    {
      closeQuietly( ois );
      closeQuietly( fis );
    }
  }

  /**
   * Stores the contents of this repository to a given {@link File}.
   *
   * @param repoFile
   *          the repository file to store the data to, cannot be
   *          <code>null</code>.
   * @throws IOException
   *           in acse of I/O problems writing to the repository file.
   */
  public void store( final File repoFile ) throws IOException
  {
    FileOutputStream fos = null;
    ObjectOutputStream oos = null;

    try
    {
      fos = new FileOutputStream( repoFile );
      oos = new ObjectOutputStream( fos );

      oos.writeObject( this.mapping );
    }
    finally
    {
      closeQuietly( oos );
      closeQuietly( fos );
    }
  }

  private String createIdentity( final ConfigFileResource resource )
  {
    String factoryPid = resource.getFactoryPid();
    if ( factoryPid != null )
    {
      return String.format( "%s-%s", factoryPid, resource.getPid() );
    }
    return resource.getPid();
  }

  private boolean isNewOrChanged( final ConfigFileResource resource )
  {
    String pid = resource.getPid();
    ResourceInfo info = this.mapping.get( pid );
    return ( info == null ) || ( info.lastModified != resource.getLastModified() );
  }
}
