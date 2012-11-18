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
package nl.lxtreme.ols.client;


import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.cm.*;


/**
 * Provides a managed service factor for color schemes, allowing new color
 * schemes to be defined by means of adding a configuration file.
 */
@SuppressWarnings( "rawtypes" )
public class ColorSchemeManager implements ManagedServiceFactory
{
  // CONSTANTS

  /** The service PID of the factory itself. */
  public static final String PID = "ols.ui.colorscheme";

  private static final String SCHEME_NAME = "ols.color.scheme.name";

  // VARIABLES

  private final ConcurrentMap<String, Properties> colorSchemes = new ConcurrentHashMap<String, Properties>();

  private volatile String schemaName;

  // METHODS

  /**
   * Applies the color scheme identified by the given name.
   * 
   * @param aName
   *          the name of the schema to apply, cannot be <code>null</code> or
   *          empty.
   */
  public void applyColorScheme( final String aName )
  {
    if ( this.schemaName != null )
    {
      removeColorScheme( this.schemaName );
    }
    this.schemaName = aName;

    Properties props = getColorScheme( aName );
    if ( props == null )
    {
      return;
    }

    for ( Object key : props.keySet() )
    {
      Object value = props.get( key );
      if ( value instanceof Color )
      {
        UIManager.put( key, value );
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deleted( final String aPid )
  {
    this.colorSchemes.remove( aPid );
  }

  /**
   * @return the number of color schemes, >= 0.
   */
  public int getColorSchemeCount()
  {
    return this.colorSchemes.size();
  }

  /**
   * @return a list with color schemes, never <code>null</code>.
   */
  public List<String> getColorSchemes()
  {
    List<String> result = new ArrayList<String>();
    for ( Properties props : this.colorSchemes.values() )
    {
      result.add( props.getProperty( SCHEME_NAME ) );
    }
    Collections.sort( result );
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return "Color Scheme manager";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updated( final String aPid, final Dictionary aProperties ) throws ConfigurationException
  {
    this.colorSchemes.put( aPid, parseColorScheme( aProperties ) );
  }

  /**
   * Retrieves the color scheme by its name.
   * 
   * @param aName
   *          the name of the color scheme, can be <code>null</code> or empty.
   * @return a {@link Properties} map with the requested color scheme, or
   *         <code>null</code> if no such scheme exists.
   */
  private Properties getColorScheme( final String aName )
  {
    for ( Properties props : this.colorSchemes.values() )
    {
      if ( props.getProperty( SCHEME_NAME ).equals( aName ) )
      {
        Properties result = new Properties();
        result.putAll( props );
        return result;
      }
    }
    return null;
  }

  /**
   * Parses the given dictionary as color scheme and returns it as a
   * {@link Properties} object.
   * 
   * @param aDictionary
   *          the dictionary to parse, cannot be <code>null</code>.
   * @return a {@link Properties} object with the parsed color scheme, never
   *         <code>null</code>.
   */
  private Properties parseColorScheme( final Dictionary aDictionary ) throws ConfigurationException
  {
    String name = ( String )aDictionary.get( SCHEME_NAME );
    if ( ( name == null ) || "".equals( name.trim() ) )
    {
      throw new ConfigurationException( SCHEME_NAME, "Missing or invalid value!" );
    }

    Properties properties = new Properties();

    Enumeration keyEnum = aDictionary.keys();
    while ( keyEnum.hasMoreElements() )
    {
      String key = ( String )keyEnum.nextElement();
      if ( !key.startsWith( "ols." ) )
      {
        continue;
      }

      Object value = aDictionary.get( key );
      if ( key.endsWith( ".color" ) )
      {
        value = ColorUtils.parseColor( value.toString() );
      }

      properties.put( key, value );
    }

    return properties;
  }

  /**
   * Removes the color schema identified by the given name.
   * 
   * @param aName
   *          the name of the schema to remove the keys from, cannot be
   *          <code>null</code>.
   */
  private void removeColorScheme( final String aName )
  {
    Properties props = getColorScheme( aName );
    if ( props == null )
    {
      return;
    }

    for ( Object key : props.keySet() )
    {
      Object value = props.get( key );
      if ( value instanceof Color )
      {
        UIManager.put( key, "" );
      }
    }
  }
}
