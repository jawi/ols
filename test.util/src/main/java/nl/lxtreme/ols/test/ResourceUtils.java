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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.test;


import java.io.*;
import java.net.*;


/**
 * @author jawi
 */
public final class ResourceUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new ResourceUtils instance.
   */
  private ResourceUtils()
  {
    // NO-op
  }

  // METHODS

  /**
   * Returns the resource with the given name from the "datafiles"-directory.
   * 
   * @param aClass
   *          the class to which the resource should be located, cannot be
   *          <code>null</code>;
   * @param aName
   *          the resource name, including file extension, cannot be
   *          <code>null</code>.
   * @return the URI pointing to the requested resource, never <code>null</code>
   *         .
   */
  public static URL getResource( final Class<?> aClass, final String aName )
  {
    return getResource( aClass, "datafiles", aName );
  }

  /**
   * Returns the resource with the given name from the given base directory.
   * 
   * @param aClass
   *          the class to which the resource should be located, cannot be
   *          <code>null</code>;
   * @param aBaseDir
   *          the base directory where the resource should be located, cannot be
   *          <code>null</code>;
   * @param aName
   *          the resource name, including file extension, cannot be
   *          <code>null</code>.
   * @return the URI pointing to the requested resource, never <code>null</code>
   *         .
   */
  public static URL getResource( final Class<?> aClass, final String aBaseDir, final String aName )
  {
    final URL resource = aClass.getClassLoader().getResource( aBaseDir + File.separator + aName );
    if ( resource == null )
    {
      throw new RuntimeException( "Resource not found: " + aName );
    }
    return resource;
  }
}
