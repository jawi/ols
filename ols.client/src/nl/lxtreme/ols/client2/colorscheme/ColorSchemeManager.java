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
package nl.lxtreme.ols.client2.colorscheme;


import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;

import nl.lxtreme.ols.util.swing.*;

import org.osgi.service.cm.*;
import org.osgi.service.log.*;


/**
 * Provides a managed service factor for color schemes, allowing new color
 * schemes to be defined by means of adding a configuration file.
 */
@SuppressWarnings( "rawtypes" )
public class ColorSchemeManager implements ManagedServiceFactory, ColorSchemeProvider
{
  // CONSTANTS

  /** The service PID of the factory itself. */
  public static final String PID = "ols.ui.colorscheme";

  private static final String SCHEME_NAME = "ols.color.scheme.name";

  // VARIABLES

  private final ConcurrentMap<String, Properties> colorSchemes;
  // Injected by Felix DM...
  private volatile LogService log;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ColorSchemeManager} instance.
   */
  public ColorSchemeManager()
  {
    this.colorSchemes = new ConcurrentHashMap<String, Properties>();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void deleted( final String aPid )
  {
    Properties scheme = this.colorSchemes.remove( aPid );
    if ( scheme != null && this.log != null )
    {
      this.log.log( LogService.LOG_INFO, "Removed color scheme " + scheme.getProperty( SCHEME_NAME ) + "..." );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Properties getColorScheme( String aName )
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
   * {@inheritDoc}
   */
  @Override
  public String[] getColorSchemeNames()
  {
    List<String> result = new ArrayList<String>();
    for ( Properties props : this.colorSchemes.values() )
    {
      result.add( props.getProperty( SCHEME_NAME ) );
    }
    Collections.sort( result );
    return result.toArray( new String[result.size()] );
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
    Properties scheme = parseColorScheme( aProperties );

    this.colorSchemes.put( aPid, scheme );

    if ( this.log != null )
    {
      this.log.log( LogService.LOG_INFO, "Added/updated color scheme " + scheme.getProperty( SCHEME_NAME ) + "..." );
    }
  }

  private Color parseColor( Dictionary aDictionary, String key )
  {
    String value = aDictionary.get( key ).toString().trim();
    if ( value.startsWith( "${" ) && value.endsWith( "}" ) )
    {
      return parseColor( aDictionary, value.substring( 2, value.length() - 1 ) );
    }
    return ColorUtils.parseColor( value );
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

      if ( key.endsWith( ".color" ) )
      {
        properties.put( key, parseColor( aDictionary, key ) );
      }
      else
      {
        properties.put( key, aDictionary.get( key ) );
      }
    }

    return properties;
  }
}
