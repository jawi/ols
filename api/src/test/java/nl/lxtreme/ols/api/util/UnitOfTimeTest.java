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

import org.junit.*;


/**
 * Test cases for {@link UnitOfTime}.
 */
public class UnitOfTimeTest
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.UnitOfTime#format(double, int)}.
   */
  @Test
  public void testFormatAutoScaleOk()
  {
    assertEquals( "0.00s", UnitOfTime.format( 0 ) );
    assertEquals( "1.00ms", UnitOfTime.format( 1.0e-3 ) );
    assertEquals( "-1.00ms", UnitOfTime.format( -1.0e-3 ) );
    assertEquals( "1.20ns", UnitOfTime.format( 1.2e-9 ) );
    assertEquals( "1.00s", UnitOfTime.format( 1.0 ) );
    assertEquals( "1000.00s", UnitOfTime.format( 1.0e3 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.UnitOfTime#formatHumanReadable(double)}.
   */
  @Test
  public void testFormatHumanReadableOk()
  {
    assertEquals( "0.0s", UnitOfTime.S.formatHumanReadable( 0 ) );
    assertEquals( "1.0ms", UnitOfTime.MS.formatHumanReadable( 1.0e-3 ) );
    assertEquals( "-1.0ms", UnitOfTime.MS.formatHumanReadable( -1.0e-3 ) );
    assertEquals( "1.2345ns", UnitOfTime.NS.formatHumanReadable( 1.2345e-9 ) );
    assertEquals( "1.00001s", UnitOfTime.S.formatHumanReadable( 1.00001 ) );
    assertEquals( "1050.607s", UnitOfTime.S.formatHumanReadable( 1.050607e3 ) );
  }

  /**
   * Test method for {@link nl.lxtreme.ols.api.util.UnitOfTime#predecessor()}.
   */
  @Test
  public void testPredecessor()
  {
    assertNull( UnitOfTime.S.predecessor() );
    assertNotNull( UnitOfTime.MS.predecessor() );
    assertNotNull( UnitOfTime.US.predecessor() );
    assertNotNull( UnitOfTime.NS.predecessor() );
    assertNotNull( UnitOfTime.PS.predecessor() );
    assertNotNull( UnitOfTime.FS.predecessor() );
  }

  /**
   * Test method for {@link nl.lxtreme.ols.api.util.UnitOfTime#successor()}.
   */
  @Test
  public void testSuccessor()
  {
    assertNotNull( UnitOfTime.S.successor() );
    assertNotNull( UnitOfTime.MS.successor() );
    assertNotNull( UnitOfTime.US.successor() );
    assertNotNull( UnitOfTime.NS.successor() );
    assertNotNull( UnitOfTime.PS.successor() );
    assertNull( UnitOfTime.FS.successor() );
  }
}
