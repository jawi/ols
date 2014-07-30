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
package nl.lxtreme.ols.common;


import static nl.lxtreme.ols.common.Unit.Time.*;
import static nl.lxtreme.ols.common.Unit.Value.*;
import static org.junit.Assert.*;
import nl.lxtreme.ols.common.Unit.*;

import org.junit.*;


/**
 * Test cases for {@link Time}.
 */
public class TimeUnitTest
{
  // METHODS

  /**
   * Tests {@link Value#asTime(Number)} and {@link Value#asTime(Number, Time)}.
   */
  @Test
  public void testAsTimeOk()
  {
    assertEquals( new Value( 1.0, S ), asTime( 1.0 ) );
    assertEquals( new Value( 1.0, MS ), asTime( 1.0e-3 ) );
    assertEquals( new Value( 1.0, US ), asTime( 1.0e-6 ) );
    assertEquals( new Value( 1.0e6, MS ), asTime( 1.0e6, MS ) );
  }

  /**
   * Tests {@link Value#asTime(Number)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAsTimeWithNullUnitFail()
  {
    asTime( 1.0, null );
  }

  /**
   * Tests {@link Value#asTime(Number)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAsTimeWithNullValueFail()
  {
    asTime( null );
  }

  /**
   * Tests {@link Time#convert(double, SizeSI, SizeSI)}.
   */
  @Test
  public void testConvertToUnitOk()
  {
    assertEquals( asTime( 1.0, S ), asTime( 1.0, S ).convert( S ) );

    assertEquals( asTime( 1.0e-3, MS ), asTime( 1.0, S ).convert( MS ) );
    assertEquals( asTime( 1.0e3, S ), asTime( 1.0, MS ).convert( S ) );

    assertEquals( asTime( 1.0e-6, US ), asTime( 1.0, S ).convert( US ) );
    assertEquals( asTime( 1.0e6, S ), asTime( 1.0, US ).convert( S ) );

    assertEquals( asTime( 1.0e-9, NS ), asTime( 1.0, S ).convert( NS ) );
    assertEquals( asTime( 1.0e9, S ), asTime( 1.0, NS ).convert( S ) );

    assertEquals( asTime( 1.0e-12, PS ), asTime( 1.0, S ).convert( PS ) );
    assertEquals( asTime( 1.0e12, S ), asTime( 1.0, PS ).convert( S ) );

    assertEquals( asTime( 1.0e-15, FS ), asTime( 1.0, S ).convert( FS ) );
    assertEquals( asTime( 1.0e15, S ), asTime( 1.0, FS ).convert( S ) );

    assertEquals( asTime( 1.0e-6, NS ), asTime( 1.0, MS ).convert( NS ) );
    assertEquals( asTime( 1.0e6, MS ), asTime( 1.0, NS ).convert( MS ) );
  }

  /**
   * Tests {@link Value#convert(Unit)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testConvertWithNullToUnitFail()
  {
    asTime( 1.0 ).convert( null );
  }

  /**
   * Tests {@link Value#formatTo(java.util.Formatter, int, int, int)} for
   * {@link SizeSI} units.
   */
  @Test
  public void testFormatSizeSIOk()
  {
    assertEquals( "0.0 s", String.format( "%s", Value.asTime( 0.0 ) ) );
    assertEquals( "1.23 s", String.format( "%s", new Value( 1.23, S ) ) );
    assertEquals( "1.230 s", String.format( "%.3s", new Value( 1.23, S ) ) );
    assertEquals( " 1.23 s", String.format( "%5.2s", new Value( 1.23, S ) ) );
    assertEquals( "1.23  s", String.format( "%-5.2s", new Value( 1.23, S ) ) );
    assertEquals( "1 s", String.format( "%.0s", new Value( 1.23, S ) ) );
  }

  /**
   * Test method for {@link nl.lxtreme.ols.api.util.Time#predecessor()}.
   */
  @Test
  public void testPredecessor()
  {
    assertNull( S.predecessor() );
    assertNotNull( MS.predecessor() );
    assertNotNull( US.predecessor() );
    assertNotNull( NS.predecessor() );
    assertNotNull( PS.predecessor() );
    assertNotNull( FS.predecessor() );
  }

  /**
   * Test method for {@link nl.lxtreme.ols.api.util.Time#successor()}.
   */
  @Test
  public void testSuccessor()
  {
    assertNotNull( S.successor() );
    assertNotNull( MS.successor() );
    assertNotNull( US.successor() );
    assertNotNull( NS.successor() );
    assertNotNull( PS.successor() );
    assertNull( FS.successor() );
  }
}
