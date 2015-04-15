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


import static nl.lxtreme.ols.config.provision.impl.ConfiguratorImpl.closeQuietly;
import java.io.*;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Properties;


/**
 * Represents a Property-file resource.
 * <p>
 * This code is taken and modified from Amdatu-Configurator.
 * </p>
 */
public class ConfigFileResource
{
  // VARIABLES

  private final File resource;
  private final String factoryPid;
  private final String pid;
  private final Dictionary<String, Object> config;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ConfigFileResource} instance.
   *
   * @param aResource
   *          the configuration file, cannot be <code>null</code>.
   * @throws IOException
   *           in case of I/O problems reading from the given file.
   */
  public ConfigFileResource( final File aResource ) throws IOException
  {
    this.resource = aResource;

    FileInputStream fis = null;

    try
    {
      fis = new FileInputStream( aResource );

      // Read the entry as MetaType file...
      Properties props = new Properties();
      props.load( fis );

      String name = stripFileExtension( aResource.getName() );

      int idx = name.indexOf( '-' );
      if ( idx >= 0 )
      {
        // Service Factory PID...
        this.factoryPid = name.substring( 0, idx );
        this.pid = name.substring( idx + 1 );
      }
      else
      {
        // Service PID...
        this.factoryPid = null;
        this.pid = name;
      }

      Dictionary<String, Object> dict = new Hashtable<String, Object>( props.size() );
      for ( Object key : props.keySet() )
      {
        dict.put( ( String )key, getValue( props, ( String )key ) );
      }
      this.config = dict;
    }
    finally
    {
      closeQuietly( fis );
    }
  }

  // METHODS

  static Object getValue( final Properties properties, final String key )
  {
    Object val = properties.get( key );
    if ( val instanceof String )
    {
      String ref = ( String )val;
      if ( ref.startsWith( "${" ) && ref.endsWith( "}" ) )
      {
        return getValue( properties, ref.substring( 2, ref.length() - 1 ) );
      }
    }
    return val;
  }

  static boolean isEmtpy( final String str )
  {
    return ( str == null ) || "".equals( str.trim() );
  }

  static String stripFileExtension( final String str )
  {
    if ( isEmtpy( str ) )
    {
      return "";
    }
    int idx = str.lastIndexOf( '.' );
    if ( idx >= 0 )
    {
      String part = str.substring( idx + 1 );
      if ( "cfg".equals( part ) )
      {
        return str.substring( 0, idx );
      }
    }
    return str;
  }

  /**
   * @return the actual configuration properties, never <code>null</code>.
   */
  public Dictionary<String, ?> getConfiguration()
  {
    return this.config;
  }

  /**
   * @return the managed service factory PID, can be <code>null</code> in case
   *         this property file represents a singleton configuration.
   */
  public String getFactoryPid()
  {
    return this.factoryPid;
  }

  /**
   * @return a "unique" identifier for this Property-file resource resource,
   *         never <code>null</code>.
   */
  public String getId()
  {
    String id = getFactoryPid();
    if ( isEmtpy( id ) )
    {
      id = getPid();
    }
    return id;
  }

  /**
   * @return the last modification timestamp, in milliseconds since the Epoch.
   */
  public long getLastModified()
  {
    return this.resource.lastModified();
  }

  /**
   * @return the managed service PID, never <code>null</code>.
   */
  public String getPid()
  {
    return this.pid;
  }

  /**
   * @return <code>true</code> if this Property-file resource represents a
   *         factory configuration, <code>false</code> if it represents a
   *         "singleton" configuration.
   */
  public boolean isFactoryConfig()
  {
    return !isEmtpy( this.factoryPid );
  }
}
