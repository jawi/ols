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


import java.util.*;

import nl.lxtreme.ols.api.util.Unit.SizeSI;

import org.junit.*;


/**
 * Test cases for {@link SizeSI}.
 */
public class SizeUnitTest extends UnitTestBase
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
   * {@link nl.lxtreme.ols.api.util.SizeUnit#format(double, int)}.
   */
  @Test
  public void testFormatAutoScaleOk()
  {
    assertUnitEquals( "0.00", "B", SizeSI.format( 0 ) );
    assertUnitEquals( "10.00", "kB", SizeSI.format( 10240 ) );
    assertUnitEquals( "10.00", "MB", SizeSI.format( 10240 * 1024 ) );
    assertUnitEquals( "-10.00", "MB", SizeSI.format( -10240 * 1024 ) );
  }
}
