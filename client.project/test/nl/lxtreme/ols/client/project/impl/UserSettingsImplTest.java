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
package nl.lxtreme.ols.client.project.impl;


import static org.junit.Assert.*;

import java.awt.*;
import java.util.*;

import nl.lxtreme.ols.api.*;

import org.junit.*;


/**
 * Test cases for {@link UserSettingsImpl}.
 */
public class UserSettingsImplTest
{
  // INNER TYPES

  static enum TestEnum
  {
    A, B;
  }

  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#UserSettingsImpl(nl.lxtreme.ols.api.UserSettings)}
   * .
   */
  @Test
  public void testCopyConstructorOk()
  {
    UserSettingsImpl settings1 = new UserSettingsImpl( "test" );
    settings1.put( "key", "value" );

    UserSettingsImpl settings2 = new UserSettingsImpl( settings1 );
    assertEquals( "value", settings2.get( "key", "default" ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#delete(java.lang.String)}
   * .
   */
  @Test
  public void testDelete()
  {
    final UserSettingsImpl settings = new UserSettingsImpl( "foo" );

    settings.put( "key", "value" );
    assertEquals( "value", settings.get( "key", "default" ) );
    settings.delete( "key" );
    assertEquals( "default", settings.get( "key", "default" ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#getColor(java.lang.String, java.awt.Color)}
   * .
   */
  @Test
  public void testGetColor()
  {
    final UserSettingsImpl settings = new UserSettingsImpl( "foo" );

    settings.putColor( "key", Color.WHITE );
    assertEquals( Color.WHITE, settings.getColor( "key", Color.BLACK ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#getEnumValue(java.lang.String, java.lang.Enum)}
   * .
   */
  @Test
  public void testGetEnumValue()
  {
    final UserSettingsImpl settings = new UserSettingsImpl( "foo" );

    settings.putEnumValue( "key", TestEnum.A );
    assertEquals( TestEnum.A, settings.getEnumValue( "key", TestEnum.B ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#getName()}.
   */
  @Test
  public void testGetName()
  {
    final UserSettingsImpl settings = new UserSettingsImpl( "foo" );
    assertEquals( "foo", settings.getName() );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#UserSettingsImpl(java.lang.String)}
   * .
   */
  @Test
  public void testNameConstructorOk()
  {
    assertNotNull( new UserSettingsImpl( "test" ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#UserSettingsImpl(nl.lxtreme.ols.api.UserSettings)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testNullCopyConstructorFail()
  {
    new UserSettingsImpl( ( UserSettings )null );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#UserSettingsImpl(java.lang.String)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testNullNameConstructorFail()
  {
    assertNotNull( new UserSettingsImpl( ( String )null ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#UserSettingsImpl(java.lang.String, java.util.Properties)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testNullPropertiesConstructorFail()
  {
    new UserSettingsImpl( "test", null );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#UserSettingsImpl(java.lang.String, java.util.Properties)}
   * .
   */
  @Test
  public void testPropertiesConstructorOk()
  {
    final Properties props = new Properties();
    props.put( "key", "value" );

    final UserSettingsImpl settings = new UserSettingsImpl( "test", props );
    assertEquals( "value", settings.get( "key", "default" ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#putColor(java.lang.String, java.awt.Color)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testPutNullColorFail()
  {
    new UserSettingsImpl( "foo" ).putColor( "test", null );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.client.data.settings.UserSettingsImpl#putEnumValue(java.lang.String, java.lang.Enum)}
   * .
   */
  @Test( expected = IllegalArgumentException.class )
  public void testPutNullEnumValueFail()
  {
    new UserSettingsImpl( "foo" ).putEnumValue( "test", ( TestEnum )null );
  }

}
