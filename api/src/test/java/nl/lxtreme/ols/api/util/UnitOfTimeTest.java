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
package nl.lxtreme.ols.api.util;


import static org.junit.Assert.*;

import java.util.*;

import nl.lxtreme.ols.api.util.Unit.Time;

import org.junit.*;


/**
 * Test cases for {@link Time}.
 */
public class UnitOfTimeTest extends UnitTestBase
{
  // METHODS

  /**
   * Forces the use of English locales for this test.
   */
  @Before
  public void setUp()
  {
    Locale.setDefault( Locale.US );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.UnitOfTime#format(double, int)}.
   */
  @Test
  public void testFormatAutoScaleOk()
  {
    assertUnitEquals( "0.00", "s", Time.format( 0 ) );
    assertUnitEquals( "1.00", "ms", Time.format( 1.0e-3 ) );
    assertUnitEquals( "-1.00", "ms", Time.format( -1.0e-3 ) );
    assertUnitEquals( "1.20", "ns", Time.format( 1.2e-9 ) );
    assertUnitEquals( "1.00", "s", Time.format( 1.0 ) );
    assertUnitEquals( "1000.00", "s", Time.format( 1.0e3 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.Time#formatHumanReadable(double)}.
   */
  @Test
  public void testFormatHumanReadableOk()
  {
    assertUnitEquals( "0.0", "s", Time.S.formatHumanReadable( 0 ) );
    assertUnitEquals( "1.0", "ms", Time.MS.formatHumanReadable( 1.0e-3 ) );
    assertUnitEquals( "-1.0", "ms", Time.MS.formatHumanReadable( -1.0e-3 ) );
    assertUnitEquals( "1.2345", "ns", Time.NS.formatHumanReadable( 1.2345e-9 ) );
    assertUnitEquals( "1.00001", "s", Time.S.formatHumanReadable( 1.00001 ) );
    assertUnitEquals( "1050.607", "s", Time.S.formatHumanReadable( 1.050607e3 ) );
  }

  /**
   * Test method for {@link nl.lxtreme.ols.api.util.Time#predecessor()}.
   */
  @Test
  public void testPredecessor()
  {
    assertNull( Time.S.predecessor() );
    assertNotNull( Time.MS.predecessor() );
    assertNotNull( Time.US.predecessor() );
    assertNotNull( Time.NS.predecessor() );
    assertNotNull( Time.PS.predecessor() );
    assertNotNull( Time.FS.predecessor() );
  }

  /**
   * Test method for {@link nl.lxtreme.ols.api.util.Time#successor()}.
   */
  @Test
  public void testSuccessor()
  {
    assertNotNull( Time.S.successor() );
    assertNotNull( Time.MS.successor() );
    assertNotNull( Time.US.successor() );
    assertNotNull( Time.NS.successor() );
    assertNotNull( Time.PS.successor() );
    assertNull( Time.FS.successor() );
  }
}
