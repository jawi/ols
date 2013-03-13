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

import nl.lxtreme.ols.api.util.Unit.Frequency;

import org.junit.*;


/**
 * Test cases for {@link Frequency}.
 */
public class FrequencyUnitTest extends UnitTestBase
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
   * {@link Frequency#format(double, int)}.
   */
  @Test
  public void testFormatAutoScaleOk()
  {
    assertUnitEquals( "0.000", "Hz", Frequency.format( 0 ) );
    assertUnitEquals( "1.000", "mHz", Frequency.format( 1.0e-3 ) );
    assertUnitEquals( "1.000", "Hz", Frequency.format( 1.0 ) );
    assertUnitEquals( "1.000", "kHz", Frequency.format( 1.0e3 ) );
    assertUnitEquals( "1.000", "MHz", Frequency.format( 1.0e6 ) );
    assertUnitEquals( "1.000", "GHz", Frequency.format( 1.0e9 ) );
    assertUnitEquals( "1.000", "THz", Frequency.format( 1.0e12 ) );
    assertUnitEquals( "-1.000", "THz", Frequency.format( -1.0e12 ) );
    assertUnitEquals( "100.000", "THz", Frequency.format( 1.0e14 ) );
    assertUnitEquals( "333.333", "kHz", Frequency.format( 333333.3333333333 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.Frequency#format(double, int)}.
   */
  @Test
  public void testFormatOk()
  {
    assertUnitEquals( "0.000", "Hz", Frequency.GHZ.format( 0, 3 ) );
    assertUnitEquals( "1.000", "mHz", Frequency.MiHZ.format( 1.0e-3, 3 ) );
    assertUnitEquals( "0.001", "kHz", Frequency.KHZ.format( 1.0, 3 ) );
    assertUnitEquals( "1.000", "kHz", Frequency.KHZ.format( 1.0e3, 3 ) );
    assertUnitEquals( "1.000", "MHz", Frequency.MHZ.format( 1.0e6, 3 ) );
    assertUnitEquals( "1.000", "GHz", Frequency.GHZ.format( 1.0e9, 3 ) );
    assertUnitEquals( "1.000", "THz", Frequency.THZ.format( 1.0e12, 3 ) );
    assertUnitEquals( "100.000", "THz", Frequency.THZ.format( 1.0e14, 3 ) );
    assertUnitEquals( "333.333", "kHz", Frequency.KHZ.format( 333333.3333333333, 3 ) );
  }
}
