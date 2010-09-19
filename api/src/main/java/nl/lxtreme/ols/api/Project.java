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

import org.osgi.service.prefs.*;


/**
 * Project maintains a global properties list for all registered objects
 * implementing {@link Configurable}. It also provides methods for loading and
 * storing these properties from and to project configuration files. This allows
 * to keep multiple sets of user settings across multiple instance lifecycles.
 * 
 * @version 0.7
 * @author Michael "Mr. Sump" Poppitz
 */
public final class Project
{
  // INNER TYPES

  /**
   * @author jawi
   */
  static interface PreferenceNodeVisitor
  {
    /**
     * @param aNode
     * @throws BackingStoreException
     */
    public void visit( final Preferences aNode ) throws BackingStoreException;
  }

  // CONSTRUCTORS

  /**
   * Constructs a new project, never used.
   */
  private Project()
  {
    // NO-op
  }

  // METHODS

  /**
   * Loads properties from the given file and notifies all registered
   * configurable objects.
   * 
   * @param aFile
   *          file to read properties from
   * @throws IOException
   *           when IO operation failes
   */
  public static void load( final File aFile, final Preferences aPreferences ) throws IOException
  {
    final Properties properties = new Properties();

    final Reader reader = new FileReader( aFile );
    try
    {
      properties.load( reader );
    }
    finally
    {
      reader.close();
    }

    for ( Map.Entry<Object, Object> entry : properties.entrySet() )
    {
      final String key = ( String )entry.getKey();
      final String value = ( String )entry.getValue();

      final String[] keyEntry = key.split( "/", 2 );
      aPreferences.node( keyEntry[0] ).put( keyEntry[1], value );
    }
  }

  /**
   * Stores properties fetched from all registered configurable objects in the
   * given file.
   * 
   * @param aFile
   *          file to store properties in
   * @throws IOException
   *           when IO operation failes
   */
  public static void store( final File aFile, final Preferences aPreferences ) throws IOException
  {
    final Properties properties = new Properties();

    try
    {
      traversePreferences( aPreferences, new PreferenceNodeVisitor()
      {
        @Override
        public void visit( final Preferences aNode ) throws BackingStoreException
        {
          final String prefix = aNode.name();
          for ( String key : aNode.keys() )
          {
            properties.put( prefix + "/" + key, aNode.get( key, null ) );
          }
        }
      } );

      final Writer writer = new FileWriter( aFile );

      properties.store( writer, "OpenBench Logic Analyzer Client Project File" );

      writer.flush();
      writer.close();
    }
    catch ( BackingStoreException exception )
    {
      throw new IOException( "Failed to store project!", exception );
    }
  }

  /**
   * Visits all nodes in the given preferences tree.
   * 
   * @param aPreferences
   *          the preferences tree to visit;
   * @param aVisitor
   *          the visitor that wants to visit each node in the tree.
   * @throws BackingStoreException
   *           in case of problems accessing the preferences tree.
   */
  private static void traversePreferences( final Preferences aPreferences, final PreferenceNodeVisitor aVisitor )
      throws BackingStoreException
  {
    aVisitor.visit( aPreferences );

    for ( String childName : aPreferences.childrenNames() )
    {
      traversePreferences( aPreferences.node( childName ), aVisitor );
    }
  }
}
