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
package nl.lxtreme.ols.util;


import org.junit.*;

import java.util.*;


/**
 * 
 */
public class DisplayUtilsTest
{
  // METHODS

  /**
   * Sets up the tests.
   */
  @Before
  public void setUp()
  {
    // To ensure formatting occurs deterministically...
    Locale.setDefault( Locale.ENGLISH );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.DisplayUtils#displayFrequency(double)}.
   */
  @Test
  public void testDisplayFrequency()
  {
    // assertEquals( "0.001 Hz", DisplayUtils.displayFrequency( 1.0e-3 ) );
    // assertEquals( "1.000 Hz", DisplayUtils.displayFrequency( 1.0 ) );
    // assertEquals( "1.000 kHz", DisplayUtils.displayFrequency( 1.0e3 ) );
    // assertEquals( "1.000 MHz", DisplayUtils.displayFrequency( 1.0e6 ) );
    // assertEquals( "1.000 GHz", DisplayUtils.displayFrequency( 1.0e9 ) );
    // assertEquals( "1.000 THz", DisplayUtils.displayFrequency( 1.0e12 ) );
    // assertEquals( "100.000 THz", DisplayUtils.displayFrequency( 1.0e14 ) );
    //
    // assertEquals( "333.333 kHz", DisplayUtils.displayFrequency(
    // 333333.3333333333 ) );
    // assertEquals( "0.000 Hz", DisplayUtils.displayFrequency( 0 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.DisplayUtils#displayTime(double)}.
   */
  @Test
  public void testDisplayTime()
  {
    // assertEquals( "1000.000 s", DisplayUtils.displayTime( 1.0e3 ) );
    // assertEquals( "1.000 s", DisplayUtils.displayTime( 1.0 ) );
    // assertEquals( "1.000 ms", DisplayUtils.displayTime( 1.0e-3 ) );
    // assertEquals( "1.000 \u03BCs", DisplayUtils.displayTime( 1.0e-6 ) );
    // assertEquals( "1.000 ns", DisplayUtils.displayTime( 1.0e-9 ) );
    // assertEquals( "1.000 ps", DisplayUtils.displayTime( 1.0e-12 ) );
    // assertEquals( "10.000 fs", DisplayUtils.displayTime( 1.0e-14 ) );
    // assertEquals( "1.000 fs", DisplayUtils.displayTime( 1.0e-15 ) );
    //
    // assertEquals( "333.333 ms", DisplayUtils.displayTime( 0.3333333333333333
    // ) );
    // assertEquals( "-40.000 ns", DisplayUtils.displayTime( -4.0E-8 ) );
    // assertEquals( "0.000 s", DisplayUtils.displayTime( 1.0e-16 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.DisplayUtils#displayTime(double,int)}.
   */
  @Test
  public void testDisplayTimeWithVariablePrecision()
  {
    // assertEquals( "1000.0 s", DisplayUtils.displayTime( 1.0e3, 1, " " ) );
    // assertEquals( "1s", DisplayUtils.displayTime( 1.0, 0, "" ) );
    // assertEquals( "1.000000  ms", DisplayUtils.displayTime( 1.0e-3, 6, "  " )
    // );
  }
}

/* EOF */
