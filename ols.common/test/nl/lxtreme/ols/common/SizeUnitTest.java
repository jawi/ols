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


import static nl.lxtreme.ols.common.Unit.SizeSI.*;
import static nl.lxtreme.ols.common.Unit.Value.*;
import static org.junit.Assert.*;

import org.junit.*;


/**
 * Test cases for {@link SizeSI}.
 */
public class SizeUnitTest
{
  // METHODS

  /**
   * Tests {@link Value#asSizeSI(Number)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAsSizeSIWithNullUnitFail()
  {
    asSizeSI( 1.0, null );
  }

  /**
   * Tests {@link Value#asSizeSI(Number)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAsSizeSIWithNullValueFail()
  {
    asSizeSI( null );
  }

  /**
   * Tests {@link Value#asSizeSI(Number)} and
   * {@link Value#asSizeSI(Number, SizeSI)}.
   */
  @Test
  public void testAsSizeSIyOk()
  {
    assertEquals( new Value( 1.0, B ), asSizeSI( 1.0 ) );
    assertEquals( new Value( 1.0, KB ), asSizeSI( 1024.0 ) );
    assertEquals( new Value( 1.0, MB ), asSizeSI( 1048576.0 ) );
    assertEquals( new Value( 1.024e6, B ), asSizeSI( 1.024e6, B ) );
  }

  /**
   * Tests {@link SizeSI#convert(double, SizeSI, SizeSI)}.
   */
  @Test
  public void testConvertToUnitOk()
  {
    assertEquals( asSizeSI( 1.0, B ), asSizeSI( 1.0, B ).convert( B ) );

    assertEquals( asSizeSI( 1.0e-3, KB ), asSizeSI( 1.0, B ).convert( KB ) );
    assertEquals( asSizeSI( 1.0e3, B ), asSizeSI( 1.0, KB ).convert( B ) );

    assertEquals( asSizeSI( 1.0e-6, MB ), asSizeSI( 1.0, B ).convert( MB ) );
    assertEquals( asSizeSI( 1.0e6, B ), asSizeSI( 1.0, MB ).convert( B ) );

    assertEquals( asSizeSI( 1.0e-9, GB ), asSizeSI( 1.0, B ).convert( GB ) );
    assertEquals( asSizeSI( 1.0e9, B ), asSizeSI( 1.0, GB ).convert( B ) );

    assertEquals( asSizeSI( 1.0e-12, TB ), asSizeSI( 1.0, B ).convert( TB ) );
    assertEquals( asSizeSI( 1.0e12, B ), asSizeSI( 1.0, TB ).convert( B ) );

    assertEquals( asSizeSI( 1.0e-6, TB ), asSizeSI( 1.0, MB ).convert( TB ) );
    assertEquals( asSizeSI( 1.0e6, MB ), asSizeSI( 1.0, TB ).convert( MB ) );
  }

  /**
   * Tests {@link Value#convert(Unit)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testConvertWithNullToUnitFail()
  {
    asSizeSI( 1.0 ).convert( null );
  }

  /**
   * Tests {@link Value#formatTo(java.util.Formatter, int, int, int)} for
   * {@link SizeSI} units.
   */
  @Test
  public void testFormatSizeSIOk()
  {
    assertEquals( "0.0 B", String.format( "%s", Value.asSizeSI( 0.0 ) ) );
    assertEquals( "1.23 kB", String.format( "%s", new Value( 1.23, KB ) ) );
    assertEquals( "1.230 kB", String.format( "%.3s", new Value( 1.23, KB ) ) );
    assertEquals( " 1.23 kB", String.format( "%5.2s", new Value( 1.23, KB ) ) );
    assertEquals( "1.23  kB", String.format( "%-5.2s", new Value( 1.23, KB ) ) );
  }
}
