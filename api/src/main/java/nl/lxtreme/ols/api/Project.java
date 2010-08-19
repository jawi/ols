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
package nl.lxtreme.ols.api;


import java.io.*;
import java.util.*;


/**
 * Project maintains a global properties list for all registered objects
 * implementing {@link Configurable}. It also provides methods for loading and
 * storing these properties from and to project configuration files. This allows
 * to keep multiple sets of user settings across multiple instance lifecycles.
 * 
 * @version 0.7
 * @author Michael "Mr. Sump" Poppitz
 */
public class Project
{
  // VARIABLES

  private Properties properties;
  private final List<Configurable> configurableObjectList;

  // CONSTRUCTORS

  /**
   * Constructs a new project with an empty set of properties and configurable
   * objects.
   */
  public Project()
  {
    this.properties = new Properties();
    this.configurableObjectList = new LinkedList<Configurable>();
  }

  // METHODS

  /**
   * Adds a configurable object to the project. The given objects properties
   * will be read and written whenever load and store operations take place.
   * 
   * @param aConfigurable
   *          configurable object
   */
  public void addConfigurable( final Configurable aConfigurable )
  {
    if ( !this.configurableObjectList.contains( aConfigurable ) )
    {
      this.configurableObjectList.add( aConfigurable );
    }
  }

  /**
   * Clears all properties from this project.
   */
  public void clear()
  {
    this.properties = new Properties();
  }

  /**
   * Gets all currently defined properties for this project.
   * 
   * @return project properties
   */
  public Properties getProperties()
  {
    final Iterator<Configurable> i = this.configurableObjectList.iterator();
    while ( i.hasNext() )
    {
      final Configurable configurable = i.next();
      configurable.writeProperties( configurable.getClass().getName(), this.properties );
    }
    return ( this.properties );
  }

  /**
   * Loads properties from the given file and notifies all registered
   * configurable objects.
   * 
   * @param file
   *          file to read properties from
   * @throws IOException
   *           when IO operation failes
   */
  public void load( final File file ) throws IOException
  {
    final InputStream stream = new FileInputStream( file );
    this.properties.load( stream );
    final Iterator<Configurable> i = this.configurableObjectList.iterator();
    while ( i.hasNext() )
    {
      final Configurable configurable = i.next();
      configurable.readProperties( configurable.getClass().getName(), this.properties );
    }
  }

  /**
   * Removes a configurable object from the project. The given objects
   * properties will be read and written whenever load and store operations take
   * place.
   * 
   * @param configurable
   *          configurable object
   */
  public void removeConfigurable( final Configurable configurable )
  {
    this.configurableObjectList.remove( configurable );
  }

  /**
   * Stores properties fetched from all registered configurable objects in the
   * given file.
   * 
   * @param file
   *          file to store properties in
   * @throws IOException
   *           when IO operation failes
   */
  public void store( final File file ) throws IOException
  {
    // creating new properties object will remove alien properties read from
    // broken / old project files
    this.properties = new Properties();
    final Iterator<Configurable> i = this.configurableObjectList.iterator();
    while ( i.hasNext() )
    {
      ( i.next() ).writeProperties( null, this.properties );
    }
    final OutputStream stream = new FileOutputStream( file );
    this.properties.store( stream, "Sumps Logic Analyzer Project File" );
  }
}
