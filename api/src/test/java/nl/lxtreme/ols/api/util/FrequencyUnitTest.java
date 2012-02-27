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
 * 
 */
public class FrequencyUnitTest
{
  // METHODS

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.FrequencyUnit#format(double, int)}.
   */
  @Test
  public void testFormatAutoScaleOk()
  {
    assertEquals( "0.000Hz", FrequencyUnit.format( 0 ) );
    assertEquals( "1.000mHz", FrequencyUnit.format( 1.0e-3 ) );
    assertEquals( "1.000Hz", FrequencyUnit.format( 1.0 ) );
    assertEquals( "1.000kHz", FrequencyUnit.format( 1.0e3 ) );
    assertEquals( "1.000MHz", FrequencyUnit.format( 1.0e6 ) );
    assertEquals( "1.000GHz", FrequencyUnit.format( 1.0e9 ) );
    assertEquals( "1.000THz", FrequencyUnit.format( 1.0e12 ) );
    assertEquals( "-1.000THz", FrequencyUnit.format( -1.0e12 ) );
    assertEquals( "100.000THz", FrequencyUnit.format( 1.0e14 ) );
    assertEquals( "333.333kHz", FrequencyUnit.format( 333333.3333333333 ) );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.api.util.FrequencyUnit#format(double, int)}.
   */
  @Test
  public void testFormatOk()
  {
    assertEquals( "0.000Hz", FrequencyUnit.GHZ.format( 0, 3 ) );
    assertEquals( "1.000mHz", FrequencyUnit.MiHZ.format( 1.0e-3, 3 ) );
    assertEquals( "0.001kHz", FrequencyUnit.KHZ.format( 1.0, 3 ) );
    assertEquals( "1.000kHz", FrequencyUnit.KHZ.format( 1.0e3, 3 ) );
    assertEquals( "1.000MHz", FrequencyUnit.MHZ.format( 1.0e6, 3 ) );
    assertEquals( "1.000GHz", FrequencyUnit.GHZ.format( 1.0e9, 3 ) );
    assertEquals( "1.000THz", FrequencyUnit.THZ.format( 1.0e12, 3 ) );
    assertEquals( "100.000THz", FrequencyUnit.THZ.format( 1.0e14, 3 ) );
    assertEquals( "333.333kHz", FrequencyUnit.KHZ.format( 333333.3333333333, 3 ) );
  }

}
