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


import java.awt.*;
import java.beans.*;
import java.util.*;
import java.util.List;
import java.util.regex.*;

import javax.swing.*;

import nl.lxtreme.ols.util.*;

import org.osgi.service.cm.*;


/**
 * Provides a managed service that is used to initialize the {@link UIManager}
 * with its default values for the OLS client.
 */
public class UIManagerConfigurator implements ManagedService, PropertyChangeListener
{
  // CONSTANTS

  public static final String PID = "ols.ui.defaults";

  private static final String PREFIX = "ols.";
  private static final String FONT_SUFFIX = ".font";
  private static final String COLOR_SUFFIX = ".color";
  private static final String FLOAT_SUFFIX = ".float";
  private static final String ENUM_SUFFIX = ".enum";
  private static final String BOOLEAN_SUFFIX = ".boolean";

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void propertyChange( final PropertyChangeEvent aEvt )
  {
    String propertyName = aEvt.getPropertyName();
    if ( propertyName.startsWith( PREFIX ) )
    {
      System.out.println( "Property changed: " + propertyName + " (old = " + aEvt.getOldValue() + ", new = "
          + aEvt.getNewValue() + ")" );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings( "rawtypes" )
  public void updated( final Dictionary aProperties ) throws ConfigurationException
  {
    if ( aProperties == null )
    {
      // Fall back to the defaults by removing all keys from UIManager...
      removeOlsSpecificKeys();
    }
    else
    {
      // Apply the specific values to the UIManager...
      applyOlsSpecificKeys( aProperties );
    }
  }

  /**
   * Called when this component is started by the dependency manager.
   */
  protected void start()
  {
    UIManager.addPropertyChangeListener( this );
    UIManager.getDefaults().addPropertyChangeListener( this );
  }

  /**
   * Called when this component is stopped by the dependency manager.
   */
  protected void stop()
  {
    UIManager.getDefaults().removePropertyChangeListener( this );
    UIManager.removePropertyChangeListener( this );
  }

  /**
   * @param aResult
   * @param aSize
   * @return
   */
  private Font applyFontSize( final Font aResult, final String aSize )
  {
    if ( aSize.endsWith( "em" ) )
    {
      // Relative size...
      float factor = Float.parseFloat( aSize.substring( 0, aSize.length() - 2 ) );
      return aResult.deriveFont( aResult.getSize2D() * factor );
    }
    else if ( aSize.endsWith( "%" ) )
    {
      // Percentage...
      float factor = Float.parseFloat( aSize.substring( 0, aSize.length() - 1 ) );
      return aResult.deriveFont( aResult.getSize2D() * ( factor / 100.0f ) );
    }
    else
    {
      // Points...
      int pointSize = Integer.parseInt( aSize );
      return aResult.deriveFont( pointSize );
    }
  }

  /**
   * @param aResult
   * @param aVariant
   * @return
   */
  private Font applyFontVariant( final Font aResult, final String aVariant )
  {
    if ( "bold".equalsIgnoreCase( aVariant ) )
    {
      return aResult.deriveFont( Font.BOLD );
    }
    else if ( "italic".equalsIgnoreCase( aVariant ) || "oblique".equalsIgnoreCase( aVariant ) )
    {
      return aResult.deriveFont( Font.ITALIC );
    }
    return aResult;
  }

  /**
   * @param aProperties
   * @throws ConfigurationException
   */
  @SuppressWarnings( "rawtypes" )
  private void applyOlsSpecificKeys( final Dictionary aProperties ) throws ConfigurationException
  {
    final Properties config = new Properties();

    final Enumeration keys = aProperties.keys();
    while ( keys.hasMoreElements() )
    {
      String key = ( String )keys.nextElement();
      if ( !key.startsWith( PREFIX ) )
      {
        continue;
      }

      try
      {
        Object value = parseAsValue( key, aProperties.get( key ) );
        config.put( key, value );
      }
      catch ( Exception exception )
      {
        System.err.println( "Configuration problem for '" + key + "' (value = '" + aProperties.get( key ) + "')!" );
        exception.printStackTrace();
        throw new ConfigurationException( key, "Unable to parse value!", exception );
      }
    }

    // Replace all placeholders with real values...
    replacePlaceholders( config );

    // Write all values to the UIManager...
    for ( Map.Entry<Object, Object> entry : config.entrySet() )
    {
      Object key = entry.getKey();
      Object value = entry.getValue();

      UIManager.put( key, value );
    }
  }

  /**
   * @param aKey
   * @param aObject
   * @return
   */
  private Object parseAsValue( final String aKey, final Object aObject )
  {
    String value = aObject.toString().trim();
    if ( aKey.endsWith( BOOLEAN_SUFFIX ) )
    {
      // Parse as boolean value...
      return Boolean.valueOf( value );
    }
    else if ( aKey.endsWith( COLOR_SUFFIX ) )
    {
      // Parse as color
      return ColorUtils.parseColor( value );
    }
    else if ( aKey.endsWith( ENUM_SUFFIX ) )
    {
      // Parse as String-enum value...
      return value;
    }
    else if ( aKey.endsWith( FLOAT_SUFFIX ) )
    {
      // Parse as float value
      return Float.valueOf( value );
    }
    else if ( aKey.endsWith( FONT_SUFFIX ) )
    {
      // Parse as Font value...
      return parseFontValue( value );
    }
    else
    {
      // Parse as integer value
      return Integer.valueOf( value );
    }
  }

  /**
   * @param value
   * @return
   */
  private Object parseFontValue( final String value )
  {
    String regex = "\\s*(%\\{[^}]+\\}|[\\s]+)\\s*(?i)(bold|italic|plain)?\\s*([^\\s]+)?\\s*";
    Pattern p = Pattern.compile( regex );
    Matcher m = p.matcher( value );
    if ( m.matches() )
    {
      String name = m.group( 1 );
      String variant = m.group( 2 );
      String size = m.group( 3 );

      Font result;
      if ( name.startsWith( "%" ) )
      {
        result = UIManager.getFont( name.substring( 2, name.length() - 1 ) );
      }
      else
      {
        result = Font.decode( name );
      }

      if ( size != null )
      {
        result = applyFontSize( result, size );
      }

      if ( variant != null )
      {
        result = applyFontVariant( result, variant );
      }

      return result;
    }
    return null;
  }

  /**
   * 
   */
  private void removeOlsSpecificKeys()
  {
    final Enumeration<Object> keys = UIManager.getDefaults().keys();
    while ( keys.hasMoreElements() )
    {
      Object key = keys.nextElement();
      if ( key.toString().startsWith( PREFIX ) )
      {
        UIManager.put( key, null );
      }
    }
  }

  /**
   * @param aConfig
   * @throws ConfigurationException
   */
  private void replacePlaceholders( final Properties aConfig ) throws ConfigurationException
  {
    final List<Object> keySet = new ArrayList<Object>( aConfig.keySet() );
    for ( Object key : keySet )
    {
      Object value = aConfig.get( key );
      if ( ( value instanceof String ) && ( ( String )value ).matches( "\\$\\{[^}]+\\}" ) )
      {
        String v = ( String )value;
        v = v.substring( 2, v.length() - 1 );

        Object newValue = aConfig.get( v );
        if ( newValue == null )
        {
          throw new ConfigurationException( key.toString(), "Missing value for placeholder!" );
        }
        aConfig.put( key, newValue );
      }
    }
  }
}
