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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.export.vcd;


import static nl.lxtreme.ols.export.vcd.ValueChangeDumpHelper.*;
import static org.junit.Assert.*;

import java.util.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.Parameters;


/**
 * @author jawi
 */
@RunWith( Parameterized.class )
public class ValueChangeDumpTimeBaseTest
{
  // VARIABLES

  private final long inputSampleRate;
  private final double timebaseValue;
  private final String timebaseString;

  // CONSTRUCTORS

  /**
   * Creates a new ValueChangeDumpTimeBaseTest instance.
   */
  public ValueChangeDumpTimeBaseTest( final long aInputSampleRate, final double aTimebaseValue,
      final String aTimebaseString )
  {
    this.inputSampleRate = aInputSampleRate;
    this.timebaseValue = aTimebaseValue;
    this.timebaseString = aTimebaseString;
  }

  // METHODS

  /**
   * @return a collection of test data.
   */
  @Parameters
  @SuppressWarnings( "boxing" )
  public static Collection<Object[]> getTestData()
  {
    return Arrays.asList( new Object[][] { //
        // InputSampleRate, TimebaseValue, TimebaseString
            { 1, 1.0, "1 s" }, // 0
            { 10, 0.1, "100 ms" }, // 1
            { 100, 0.01, "10 ms" }, // 2
            { 1000, 1.0e-3, "1 ms" }, // 3
            { 10000, 1.0e-4, "100 us" }, // 4
            { 100000, 1.0e-5, "10 us" }, // 5
            { 1000000, 1.0e-6, "1 us" }, // 6
            { 10000000, 1.0e-7, "100 ns" }, // 7
            { 100000000, 1.0e-8, "10 ns" }, // 8
            { 1000000000, 1.0e-9, "1 ns" }, // 9
            { 10000000000L, 1.0e-10, "100 ps" }, // 10
            { 100000000000L, 1.0e-11, "10 ps" }, // 11
            { 1000000000000L, 1.0e-12, "1 ps" }, // 12
            { 10000000000000L, 1.0e-13, "100 fs" }, // 13
            { 100000000000000L, 1.0e-14, "10 fs" }, // 14
            { 1000000000000000L, 1.0e-15, "1 fs" }, // 15
            { 10000000000000000L, 1.0e-16, "100 as" }, // 16
            { 100000000000000000L, 1.0e-17, "10 as" }, // 17
            { 1000000000000000000L, 1.0e-18, "1 as" }, // 18
            // Exotic cases...
            { 1500000000L, 1.0e-10, "667 ps" }, // 20
            { 15000000L, 1.0e-8, "67 ns" }, // 21
        } );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.export.vcd.ValueChangeDumpHelper#getTimebase(long)}.
   */
  @Test
  public void testGetTimebase()
  {
    assertEquals( this.timebaseValue, getTimebase( this.inputSampleRate ), 1.0e-17 );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.export.vcd.ValueChangeDumpHelper#writeTimescale(java.io.PrintWriter, double)}
   * .
   */
  @Test
  public void testWriteTimescale()
  {
    double time = ( 1.0 / this.inputSampleRate );
    assertEquals( this.timebaseString, getTimescale( time ) );
  }

}
