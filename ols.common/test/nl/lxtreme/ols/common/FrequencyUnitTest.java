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


import static nl.lxtreme.ols.common.Unit.Frequency.*;
import static nl.lxtreme.ols.common.Unit.Value.*;
import static org.junit.Assert.*;
import nl.lxtreme.ols.common.Unit.Frequency;
import nl.lxtreme.ols.common.Unit.Value;

import org.junit.*;


/**
 * Test cases for {@link Frequency}.
 */
public class FrequencyUnitTest
{
  // METHODS

  /**
   * Tests {@link Value#asFrequency(Number)} and
   * {@link Value#asFrequency(Number, Frequency)}.
   */
  @Test
  public void testAsFrequencyOk()
  {
    assertEquals( new Value( 1.0, HZ ), asFrequency( 1.0 ) );
    assertEquals( new Value( 1.001, KHZ ), asFrequency( 1.001e3 ) );
    assertEquals( new Value( 1.0, MHZ ), asFrequency( 1.0e6 ) );
    assertEquals( new Value( 1.0e6, HZ ), asFrequency( 1.0e6, HZ ) );
  }

  /**
   * Tests {@link Value#asFrequency(Number)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAsFrequencyWithNullValueFail()
  {
    asFrequency( null );
  }

  /**
   * Tests {@link Value#asFrequency(Number, Frequency)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAsFrequencyWithNullUnitFail()
  {
    asFrequency( 1.0, null );
  }

  /**
   * Tests {@link Value#convert(Unit)} for {@link Frequency} units.
   */
  @Test
  public void testConvertToUnitOk()
  {
    assertEquals( asFrequency( 1.0, HZ ), asFrequency( 1.0, HZ ).convert( HZ ) );

    assertEquals( asFrequency( 1.0e-15, THZ ), asFrequency( 1.0, MiHZ ).convert( THZ ) );
    assertEquals( asFrequency( 1.0e15, MiHZ ), asFrequency( 1.0, THZ ).convert( MiHZ ) );

    assertEquals( asFrequency( 1.0e-12, GHZ ), asFrequency( 1.0, MiHZ ).convert( GHZ ) );
    assertEquals( asFrequency( 1.0e12, MiHZ ), asFrequency( 1.0, GHZ ).convert( MiHZ ) );

    assertEquals( asFrequency( 1.0e-9, MHZ ), asFrequency( 1.0, MiHZ ).convert( MHZ ) );
    assertEquals( asFrequency( 1.0e9, MiHZ ), asFrequency( 1.0, MHZ ).convert( MiHZ ) );

    assertEquals( asFrequency( 1.0e-6, KHZ ), asFrequency( 1.0, MiHZ ).convert( KHZ ) );
    assertEquals( asFrequency( 1.0e6, MiHZ ), asFrequency( 1.0, KHZ ).convert( MiHZ ) );

    assertEquals( asFrequency( 1.0e-3, HZ ), asFrequency( 1.0, MiHZ ).convert( HZ ) );
    assertEquals( asFrequency( 1.0e3, MiHZ ), asFrequency( 1.0, HZ ).convert( MiHZ ) );

    assertEquals( asFrequency( 1.0e-3, KHZ ), asFrequency( 1.0, HZ ).convert( KHZ ) );
    assertEquals( asFrequency( 1.0e3, HZ ), asFrequency( 1.0, KHZ ).convert( HZ ) );

    assertEquals( asFrequency( 1.0e-6, MHZ ), asFrequency( 1.0, HZ ).convert( MHZ ) );
    assertEquals( asFrequency( 1.0e6, HZ ), asFrequency( 1.0, MHZ ).convert( HZ ) );

    assertEquals( asFrequency( 1.0e-9, GHZ ), asFrequency( 1.0, HZ ).convert( GHZ ) );
    assertEquals( asFrequency( 1.0e9, HZ ), asFrequency( 1.0, GHZ ).convert( HZ ) );

    assertEquals( asFrequency( 1.0e-12, THZ ), asFrequency( 1.0, HZ ).convert( THZ ) );
    assertEquals( asFrequency( 1.0e12, HZ ), asFrequency( 1.0, THZ ).convert( HZ ) );

    assertEquals( asFrequency( 1.0e-6, THZ ), asFrequency( 1.0, MHZ ).convert( THZ ) );
    assertEquals( asFrequency( 1.0e6, MHZ ), asFrequency( 1.0, THZ ).convert( MHZ ) );
  }

  /**
   * Tests {@link Value#convert(Unit)}.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testConvertWithNullToUnitFail()
  {
    asFrequency( 1.0 ).convert( null );
  }

  /**
   * Tests {@link Value#formatTo(java.util.Formatter, int, int, int)} for
   * {@link Frequency} units.
   */
  @Ignore
  @Test
  public void testFormatFrequencyOk()
  {
    assertEquals( "0.0 Hz", String.format( "%s", Value.asFrequency( 0.0 ) ) );
    assertEquals( "1.23 kHz", String.format( "%s", new Value( 1.23, KHZ ) ) );
    assertEquals( "1.230 kHz", String.format( "%.3s", new Value( 1.23, KHZ ) ) );
    assertEquals( " 1.23 kHz", String.format( "%5.2s", new Value( 1.23, KHZ ) ) );
    assertEquals( "1.23  kHz", String.format( "%-5.2s", new Value( 1.23, KHZ ) ) );
  }
}
