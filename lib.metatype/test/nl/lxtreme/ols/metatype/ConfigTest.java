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
package nl.lxtreme.ols.metatype;


import java.util.*;

import nl.lxtreme.osgi.metatype.*;

import aQute.bnd.annotation.metatype.*;

import junit.framework.*;


/**
 * Test cases for {@link Config}.
 */
public class ConfigTest extends TestCase
{
  // INNER TYPES

  static interface AnnotationTest
  {
    // @formatter:off
    @Meta.AD( deflt = "true" )
    Boolean booleanObject();

    @Meta.AD( deflt = "1" )
    Byte byteObject();

    @Meta.AD( deflt = "Sat, 12 Aug 1995 13:30:00 GMT" )
    Date dateObject();

    @Meta.AD( deflt = "2" )
    Double doubleObject();

    @Meta.AD( deflt = "A" )
    TestEnum enumObject();

    @Meta.AD( deflt = "3" )
    Float floatObject();

    @Meta.AD( deflt = "4" )
    Integer intObject();

    @Meta.AD( deflt = "5" )
    Long longObject();

    @Meta.AD( deflt = "6" )
    Short shortObject();

    @Meta.AD( deflt = "7" )
    String stringObject();
    // @formatter:on
  }

  static interface PrimitiveTest
  {
    // @formatter:off
    Boolean booleanObject();
    boolean booleanPrimitive();
    
    Byte byteObject();
    byte bytePrimitive();
    
    Date dateObject();
    
    Double doubleObject();
    double doublePrimitive();

    TestEnum enumObject();

    Float floatObject();
    float floatPrimitive();

    Integer intObject();
    int intPrimitive();

    Long longObject();
    long longPrimitive();

    Short shortObject();
    short shortPrimitive();

    String stringObject();
    // @formatter:on
  }

  static enum TestEnum
  {
    A, B, C;
  }

  // METHODS

  /**
   * Tests that the default values defined in the annotation for
   * primitive(wrapper) return values works.
   */
  public void testAnnotationDefaultOk() throws Exception
  {
    AnnotationTest config = Config.create( AnnotationTest.class, null );

    assertEquals( Boolean.TRUE, config.booleanObject() );
    assertEquals( Byte.valueOf( "1" ), config.byteObject() );
    assertEquals( Double.valueOf( "2" ), config.doubleObject() );
    assertEquals( Float.valueOf( "3" ), config.floatObject() );
    assertEquals( Integer.valueOf( 4 ), config.intObject() );
    assertEquals( Long.valueOf( 5L ), config.longObject() );
    assertEquals( Short.valueOf( "6" ), config.shortObject() );
    assertEquals( "7", config.stringObject() );
    assertEquals( TestEnum.A, config.enumObject() );
    // new Date( 808234200000L ) = "Sat, 12 Aug 1995 13:30:00 GMT"
    assertEquals( new Date( 808234200000L ), config.dateObject() );
  }

  /**
   * Tests that the default values for primitive return values works.
   */
  public void testPrimitiveDefaultOk() throws Exception
  {
    PrimitiveTest config = Config.create( PrimitiveTest.class, null );

    assertEquals( false, config.booleanPrimitive() );
    assertEquals( ( byte )0, config.bytePrimitive() );
    assertEquals( Double.valueOf( "0" ), Double.valueOf( config.doublePrimitive() ) );
    assertEquals( Float.valueOf( "0" ), Float.valueOf( config.floatPrimitive() ) );
    assertEquals( 0, config.intPrimitive() );
    assertEquals( 0, config.longPrimitive() );
    assertEquals( ( short )0, config.shortPrimitive() );
  }

  /**
   * Tests that the default values for primitive return values works.
   */
  public void testPrimitiveWithValuesOk() throws Exception
  {
    PrimitiveTest config = Config.create( PrimitiveTest.class, getPrimitiveProperties() );

    assertEquals( true, config.booleanPrimitive() );
    assertEquals( ( byte )1, config.bytePrimitive() );
    assertEquals( Double.valueOf( "2" ), Double.valueOf( config.doublePrimitive() ) );
    assertEquals( Float.valueOf( "3" ), Float.valueOf( config.floatPrimitive() ) );
    assertEquals( 4, config.intPrimitive() );
    assertEquals( 5L, config.longPrimitive() );
    assertEquals( ( short )6, config.shortPrimitive() );
  }

  /**
   * Tests that the default values for primitive wrapper return values works.
   */
  public void testPrimitiveWrapperDefaultOk() throws Exception
  {
    PrimitiveTest config = Config.create( PrimitiveTest.class, null );

    assertEquals( Boolean.FALSE, config.booleanObject() );
    assertEquals( Byte.valueOf( "0" ), config.byteObject() );
    assertEquals( Double.valueOf( "0" ), config.doubleObject() );
    assertEquals( Float.valueOf( "0" ), config.floatObject() );
    assertEquals( Integer.valueOf( 0 ), config.intObject() );
    assertEquals( Long.valueOf( 0 ), config.longObject() );
    assertEquals( Short.valueOf( "0" ), config.shortObject() );
    assertEquals( null, config.stringObject() );
    assertEquals( null, config.enumObject() );
    assertEquals( null, config.dateObject() );
  }

  /**
   * Tests that the default values for primitive wrapper return values works.
   */
  public void testPrimitiveWrapperWithValuesOk() throws Exception
  {
    PrimitiveTest config = Config.create( PrimitiveTest.class, getObjectProperties() );

    assertEquals( Boolean.TRUE, config.booleanObject() );
    assertEquals( Byte.valueOf( "1" ), config.byteObject() );
    assertEquals( Double.valueOf( "2" ), config.doubleObject() );
    assertEquals( Float.valueOf( "3" ), config.floatObject() );
    assertEquals( Integer.valueOf( 4 ), config.intObject() );
    assertEquals( Long.valueOf( 5 ), config.longObject() );
    assertEquals( Short.valueOf( "6" ), config.shortObject() );
    assertEquals( "7", config.stringObject() );
    assertEquals( TestEnum.B, config.enumObject() );
    // new Date( 808234200000L ) = "Sat, 12 Aug 1995 13:30:00 GMT"
    assertEquals( new Date( 808234200000L ), config.dateObject() );
  }

  /**
   * @return a properties object with some primitive wrapper values.
   */
  private Properties getObjectProperties()
  {
    Properties result = new Properties();
    result.put( "booleanObject", Boolean.TRUE );
    result.put( "byteObject", Byte.valueOf( "1" ) );
    result.put( "doubleObject", Double.valueOf( "2" ) );
    result.put( "floatObject", Float.valueOf( "3" ) );
    result.put( "intObject", Integer.valueOf( "4" ) );
    result.put( "longObject", Long.valueOf( "5" ) );
    result.put( "shortObject", Short.valueOf( "6" ) );
    result.put( "stringObject", "7" );
    result.put( "enumObject", "B" );
    result.put( "dateObject", "Sat, 12 Aug 1995 13:30:00 GMT" );
    return result;
  }

  /**
   * @return a properties object with some primitive values.
   */
  private Properties getPrimitiveProperties()
  {
    Properties result = new Properties();
    result.put( "booleanPrimitive", Boolean.TRUE );
    result.put( "bytePrimitive", Byte.valueOf( "1" ) );
    result.put( "doublePrimitive", Double.valueOf( "2" ) );
    result.put( "floatPrimitive", Float.valueOf( "3" ) );
    result.put( "intPrimitive", Integer.valueOf( "4" ) );
    result.put( "longPrimitive", Long.valueOf( "5" ) );
    result.put( "shortPrimitive", Short.valueOf( "6" ) );
    return result;
  }
}
