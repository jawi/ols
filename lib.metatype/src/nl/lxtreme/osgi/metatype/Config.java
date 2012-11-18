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
package nl.lxtreme.osgi.metatype;


import java.lang.reflect.*;
import java.util.*;

import aQute.bnd.annotation.metatype.*;
import aQute.bnd.annotation.metatype.Meta.AD;


/**
 * Provides an utility class for handling MetaType configuration classes.
 */
public final class Config
{
  // INNER TYPES

  /**
   * Provides the actual handler for the configuration options.
   */
  static class ConfigHandler implements InvocationHandler
  {
    // CONSTANTS

    private static final Boolean DEFAULT_BOOLEAN = Boolean.FALSE;
    private static final Byte DEFAULT_BYTE = new Byte( "0" );
    private static final Short DEFAULT_SHORT = new Short( "0" );
    private static final Integer DEFAULT_INT = new Integer( 0 );
    private static final Long DEFAULT_LONG = new Long( 0 );
    private static final Float DEFAULT_FLOAT = new Float( 0.0f );
    private static final Double DEFAULT_DOUBLE = new Double( 0.0 );

    // VARIABLES

    private final Map<Object, Object> properties;

    // CONSTRUCTORS

    /**
     * Creates a new {@link ConfigHandler} instance.
     * 
     * @param aProperties
     *          the properties to convert, cannot be <code>null</code>.
     */
    public ConfigHandler( final Map<Object, Object> aProperties )
    {
      this.properties = aProperties;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public Object invoke( final Object aProxy, final Method aMethod, final Object[] aArgs ) throws Throwable
    {
      final AD annotation = aMethod.getAnnotation( Meta.AD.class );

      final String id = mangleMethodName( aMethod.getName() );

      Object result = this.properties.get( id );
      if ( annotation != null )
      {
        if ( result == null )
        {
          String deflt = annotation.deflt();
          if ( ( deflt != null ) && !"".equals( deflt.trim() ) )
          {
            result = deflt;
          }
        }
      }

      return convertTo( aMethod.getReturnType(), result );
    }

    /**
     * Converts a given value to requested type.
     * 
     * @param aType
     *          the type to convert the given value to, cannot be
     *          <code>null</code>;
     * @param aValue
     *          the value to convert, can be <code>null</code>.
     * @return the converted type, can be <code>null</code>.
     */
    @SuppressWarnings( { "unchecked", "rawtypes" } )
    private Object convertTo( final Class<?> aType, final Object aValue )
    {
      Object result = aValue;
      try
      {
        String strValue = aValue == null ? null : String.valueOf( aValue );
        if ( Boolean.class.equals( aType ) || Boolean.TYPE.equals( aType ) )
        {
          result = DEFAULT_BOOLEAN;
          if ( aValue != null )
          {
            result = Boolean.valueOf( strValue );
          }
        }
        else if ( Byte.class.equals( aType ) || Byte.TYPE.equals( aType ) )
        {
          result = DEFAULT_BYTE;
          if ( aValue != null )
          {
            result = Byte.valueOf( strValue );
          }
        }
        else if ( Short.class.equals( aType ) || Short.TYPE.equals( aType ) )
        {
          result = DEFAULT_SHORT;
          if ( aValue != null )
          {
            result = Short.valueOf( strValue );
          }
        }
        else if ( Integer.class.equals( aType ) || Integer.TYPE.equals( aType ) )
        {
          result = DEFAULT_INT;
          if ( aValue != null )
          {
            result = Integer.valueOf( strValue );
          }
        }
        else if ( Long.class.equals( aType ) || Long.TYPE.equals( aType ) )
        {
          result = DEFAULT_LONG;
          if ( aValue != null )
          {
            result = Long.valueOf( strValue );
          }
        }
        else if ( Float.class.equals( aType ) || Float.TYPE.equals( aType ) )
        {
          result = DEFAULT_FLOAT;
          if ( aValue != null )
          {
            result = Float.valueOf( strValue );
          }
        }
        else if ( Double.class.equals( aType ) || Double.TYPE.equals( aType ) )
        {
          result = DEFAULT_DOUBLE;
          if ( aValue != null )
          {
            result = Double.valueOf( strValue );
          }
        }
        else if ( aType.isEnum() )
        {
          if ( aValue != null )
          {
            result = Enum.valueOf( ( Class<Enum> )aType, strValue );
          }
        }
        else
        {
          try
          {
            Constructor<?> c = aType.getConstructor( String.class );
            result = c.newInstance( strValue );
          }
          catch ( Throwable t )
          {
            // Ignore...
          }
        }
      }
      catch ( NumberFormatException e )
      {
        // Result already set; ignore conversion...
      }
      return result;
    }

    /**
     * Mangles a given name into something that Bnd also uses.
     * 
     * @param aName
     *          the name to mangle, cannot be <code>null</code>.
     * @return the mangled name, never <code>null</code>.
     */
    private String mangleMethodName( final String aName )
    {
      StringBuilder sb = new StringBuilder( aName );
      for ( int i = 0; i < sb.length(); i++ )
      {
        char c = sb.charAt( i );
        boolean twice = ( i < ( sb.length() - 1 ) ) && ( sb.charAt( i + 1 ) == c );
        if ( ( c == '$' ) || ( c == '_' ) )
        {
          if ( twice )
          {
            sb.deleteCharAt( i + 1 );
          }
          else if ( c == '$' )
          {
            sb.deleteCharAt( i-- ); // Remove dollars
          }
          else
          {
            sb.setCharAt( i, '.' ); // Make _ into .
          }
        }
      }
      return sb.toString();
    }
  }

  // CONSTANTS

  static final String NULL = "\0NOT_SET\0";

  // CONSTRUCTORS

  /**
   * Creates a new {@link Config} instance.
   */
  private Config()
  {
    // Not used...
  }

  // METHODS

  /**
   * Creates a wrapper for the given class allowing the values of the given map
   * to be returned by its methods.
   * 
   * @param aClass
   *          the class to wrap, cannot be <code>null</code>;
   * @param aProperties
   *          the properties to use as values for the wrapped class values, can
   *          be <code>null</code>.
   * @return a wrapper object, never <code>null</code>.
   */
  public static <T> T create( final Class<T> aClass, final Map<Object, Object> aProperties )
  {
    Map<Object, Object> props = aProperties;
    if ( props == null )
    {
      props = new HashMap<Object, Object>();
    }

    Object proxy = Proxy.newProxyInstance( aClass.getClassLoader(), //
        new Class<?>[] { aClass }, new ConfigHandler( props ) );
    return aClass.cast( proxy );
  }
}
