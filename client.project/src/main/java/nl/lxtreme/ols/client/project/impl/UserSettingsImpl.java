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
package nl.lxtreme.ols.client.project.impl;


import java.awt.*;
import java.util.*;
import java.util.Map.Entry;

import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.util.*;


/**
 * Provides an implementation of {@link UserSettings}.
 */
public class UserSettingsImpl implements UserSettings
{
  // INNER TYPES

  /**
   * Provides a user settings iterator.
   */
  static final class UserSettingsIterator implements Iterator<Map.Entry<String, Object>>
  {
    // VARIABLES

    private final Iterator<Entry<Object, Object>> entries;

    // CONSTRUCTORS

    /**
     * Creates a new UserSettingsIterator instance.
     * 
     * @param aProperties
     *          the properties to iterate over, cannot be <code>null</code>.
     */
    public UserSettingsIterator( final Properties aProperties )
    {
      this.entries = aProperties.entrySet().iterator();
    }

    // METHODS

    /**
     * @see java.util.Iterator#hasNext()
     */
    @Override
    public boolean hasNext()
    {
      return this.entries.hasNext();
    }

    /**
     * @see java.util.Iterator#next()
     */
    @Override
    public Entry<String, Object> next()
    {
      final Entry<Object, Object> nextEntry = this.entries.next();

      final String key = String.valueOf( nextEntry.getKey() );
      final Object value = nextEntry.getValue();

      return new AbstractMap.SimpleImmutableEntry<String, Object>( key, value );
    }

    /**
     * @see java.util.Iterator#remove()
     */
    @Override
    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final String name;
  private final Properties properties;

  // CONSTRUCTORS

  /**
   * Creates a new UserSettingsImpl instance.
   * 
   * @param aName
   *          the name of this user settings, cannot be <code>null</code>.
   */
  public UserSettingsImpl( final String aName )
  {
    this( aName, new Properties() );
  }

  /**
   * Creates a new UserSettingsImpl instance.
   * 
   * @param aName
   *          the name of this user settings, cannot be <code>null</code>;
   * @param aProperties
   *          the (initial) properties of this user settings, cannot be
   *          <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given properties or name was <code>null</code>.
   */
  public UserSettingsImpl( final String aName, final Properties aProperties )
  {
    if ( aName == null )
    {
      throw new IllegalArgumentException( "Name cannot be null!" );
    }
    if ( aProperties == null )
    {
      throw new IllegalArgumentException( "Properties cannot be null!" );
    }
    this.name = aName;
    this.properties = ( Properties )aProperties.clone();
  }

  /**
   * Creates a new UserSettingsImpl instance.
   * 
   * @param aSettings
   *          the (initial) user settings to use, cannot be <code>null</code>.
   * @throws IllegalArgumentException
   *           in case the given settings object was <code>null</code>.
   */
  protected UserSettingsImpl( final UserSettings aSettings )
  {
    if ( aSettings == null )
    {
      throw new IllegalArgumentException( "Settings cannot be null!" );
    }
    this.name = aSettings.getName();
    if ( aSettings instanceof UserSettingsImpl )
    {
      this.properties = ( Properties )( ( UserSettingsImpl )aSettings ).getProperties().clone();
    }
    else
    {
      this.properties = new Properties();
      for ( Map.Entry<String, Object> entry : aSettings )
      {
        this.properties.put( entry.getKey(), entry.getValue() );
      }
    }
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public String get( final String aName, final String aDefaultValue )
  {
    String result = this.properties.getProperty( aName, aDefaultValue );
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean getBoolean( final String aName, final boolean aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }
    return Boolean.parseBoolean( value );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getInt( final String aName, final int aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }
    return Integer.parseInt( value );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public long getLong( final String aName, final long aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }
    return Long.parseLong( value );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName()
  {
    return this.name;
  }

  /**
   * Returns the properties representation of these user settings.
   * 
   * @return the properties, never <code>null</code>.
   */
  public Properties getProperties()
  {
    return this.properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<Entry<String, Object>> iterator()
  {
    return new UserSettingsIterator( this.properties );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void put( final String aName, final String aValue )
  {
    this.properties.put( aName, aValue );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putAll( final Map<?, ?> aSettings )
  {
    this.properties.putAll( aSettings );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putBoolean( final String aName, final boolean aValue )
  {
    this.properties.put( aName, Boolean.toString( aValue ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putInt( final String aName, final int aValue )
  {
    this.properties.put( aName, Integer.toString( aValue ) );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void putLong( final String aName, final long aValue )
  {
    this.properties.put( aName, Long.toString( aValue ) );
  }

  /**
   * Removes the value identified by the given name.
   * 
   * @param aName
   *          the name of the value to remove, cannot be <code>null</code>.
   */
  protected void delete( final String aName )
  {
    this.properties.remove( aName );
  }

  /**
   * Convenience method to return the color of a certain property.
   * 
   * @param aName
   * @param aDefaultValue
   * @return
   */
  protected Color getColor( final String aName, final Color aDefaultValue )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefaultValue;
    }

    return ColorUtils.parseColor( value );
  }

  /**
   * Convenience method to add a enum constant to the properties.
   * 
   * @param <T>
   *          the enum type to put into this settings object;
   * @param aName
   *          the name of which the enum constant should be retrieved;
   * @param aDefault
   *          the default value to return in case the enum constant is not set.
   * @return the enum value.
   */
  @SuppressWarnings( "unchecked" )
  protected <T extends Enum<T>> T getEnumValue( final String aName, final T aDefault )
  {
    String value = this.properties.getProperty( aName );
    if ( value == null )
    {
      return aDefault;
    }

    return ( T )Enum.valueOf( aDefault.getClass(), value );
  }

  /**
   * Convenience method to return the color of a certain property.
   * 
   * @param aName
   * @param aValue
   */
  protected void putColor( final String aName, final Color aValue )
  {
    if ( aValue == null )
    {
      throw new IllegalArgumentException( "Color cannot be null! Use delete() method to delete a key-value pair!" );
    }
    final String hexString = ColorUtils.toHexString( aValue );
    this.properties.put( aName, hexString );
  }

  /**
   * Convenience method to add a enum constant to the properties.
   * 
   * @param <T>
   *          the enum type to put into this settings object;
   * @param aName
   *          the name under which the enum constant should be stored;
   * @param aValue
   *          the enum constant to store.
   */
  protected <T extends Enum<T>> void putEnumValue( final String aName, final T aValue )
  {
    if ( aValue == null )
    {
      throw new IllegalArgumentException( "Enum cannot be null! Use delete() method to delete a key-value pair!" );
    }
    this.properties.put( aName, aValue.name() );
  }
}
