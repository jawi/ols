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
package nl.lxtreme.ols.api;


import java.io.*;
import java.util.*;


/**
 * Denotes an abstract way for obtaining/storing user settings.
 */
public interface UserSettings extends Serializable, Iterable<Map.Entry<String, Object>>
{
  // METHODS

  /**
   * Returns the string value associated with the given name, returning a given
   * default value if no value is (yet) associated.
   * 
   * @param aName
   *          the name of whose associated value should be returned;
   * @param aDefaultValue
   *          the default value to return.
   * @return the value associated with the given name, or the given default.
   */
  public String get( String aName, String aDefaultValue );

  /**
   * Returns the boolean value associated with the given name, returning a given
   * default value if no value is (yet) associated.
   * 
   * @param aName
   *          the name of whose associated value should be returned;
   * @param aDefaultValue
   *          the default value to return.
   * @return the value associated with the given name, or the given default.
   */
  public boolean getBoolean( String aName, boolean aDefaultValue );

  /**
   * Returns the integer value associated with the given name, returning a given
   * default value if no value is (yet) associated.
   * 
   * @param aName
   *          the name of whose associated value should be returned;
   * @param aDefaultValue
   *          the default value to return.
   * @return the value associated with the given name, or the given default.
   */
  public int getInt( String aName, int aDefaultValue );

  /**
   * Returns the long value associated with the given name, returning a given
   * default value if no value is (yet) associated.
   * 
   * @param aName
   *          the name of whose associated value should be returned;
   * @param aDefaultValue
   *          the default value to return.
   * @return the value associated with the given name, or the given default.
   */
  public long getLong( String aName, long aDefaultValue );

  /**
   * Returns the symbolic name for this user settings.
   * 
   * @return a symbolic name, never <code>null</code>.
   */
  public String getName();

  /**
   * Associates the given String value to the given name.
   * 
   * @param aName
   *          the name to associate the value to;
   * @param aValue
   *          the value to associate to the given name.
   */
  public void put( String aName, String aValue );

  /**
   * Allows all given map of settings to be copied to this user settings.
   * 
   * @param aSettings
   *          the map with settings to copy, cannot be <code>null</code>.
   */
  public void putAll( Map<?, ?> aSettings );

  /**
   * Associates the given boolean value to the given name.
   * 
   * @param aName
   *          the name to associate the value to;
   * @param aValue
   *          the value to associate to the given name.
   */
  public void putBoolean( String aName, boolean aValue );

  /**
   * Associates the given integer value to the given name.
   * 
   * @param aName
   *          the name to associate the value to;
   * @param aValue
   *          the value to associate to the given name.
   */
  public void putInt( String aName, int aValue );

  /**
   * Associates the given long value to the given name.
   * 
   * @param aName
   *          the name to associate the value to;
   * @param aValue
   *          the value to associate to the given name.
   */
  public void putLong( String aName, long aValue );
}
