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
package nl.lxtreme.ols.client.osgi;


import java.util.*;
import java.util.concurrent.*;

import nl.lxtreme.ols.util.*;

import org.osgi.service.cm.*;


/**
 * Provides a managed service factor for color schemes, allowing new color
 * schemes to be defined by means of adding a configuration file.
 */
@SuppressWarnings( "rawtypes" )
public class UIColorSchemeManager implements ManagedServiceFactory
{
  // CONSTANTS

  /** The service PID of the factory itself. */
  public static final String PID = "ols.ui.colorscheme";

  private static final String SCHEME_NAME = "ols.color.scheme.name";

  // VARIABLES

  private final ConcurrentMap<String, Properties> colorSchemes = new ConcurrentHashMap<String, Properties>();

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void deleted( final String aPid )
  {
    this.colorSchemes.remove( aPid );
  }

  /**
   * Retrieves the color scheme by its name.
   *
   * @param aName
   *          the name of the color scheme, can be <code>null</code> or empty.
   * @return a {@link Properties} map with the requested color scheme, or
   *         <code>null</code> if no such scheme exists.
   */
  public Properties getColorScheme( final String aName )
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
  public void updated( final String aPid, final Dictionary<String, ?> aProperties ) throws ConfigurationException
  {
    this.colorSchemes.put( aPid, parseColorScheme( aProperties ) );
  }

  private Object getPropertyValue( final Dictionary<String, ?> aProperties, final String aKey )
  {
    Object val = aProperties.get( aKey );
    if ( val instanceof String )
    {
      String ref = ( String )val;
      if ( ref.startsWith( "${" ) && ref.endsWith( "}" ) )
      {
        val = getPropertyValue( aProperties, ref.substring( 2, ref.length() - 1 ) );
      }
    }
    return val;
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
  private Properties parseColorScheme( final Dictionary<String, ?> aDictionary ) throws ConfigurationException
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

      Object value = getPropertyValue( aDictionary, key );
      if ( key.endsWith( ".color" ) )
      {
        value = ColorUtils.parseColor( value.toString() );
      }

      properties.put( key, value );
    }

    return properties;
  }
}
