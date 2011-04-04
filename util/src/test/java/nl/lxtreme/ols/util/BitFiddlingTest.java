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


import static nl.lxtreme.ols.util.NumberUtils.*;
import static org.junit.Assert.*;

import java.util.*;

import org.junit.*;
import org.junit.runner.*;
import org.junit.runners.*;
import org.junit.runners.Parameterized.*;


/**
 * Tests the reversing of bits in a byte, word and so on.
 */
@RunWith( Parameterized.class )
public class BitFiddlingTest
{
  // VARIABLES

  private final int originalValue;
  private final int bitCount;
  private final int expectedValue;

  // CONSTRUCTORS

  /**
   * @param aOriginalValue
   * @param aBitCount
   * @param aExpectedValue
   */
  public BitFiddlingTest( final int aOriginalValue, final int aBitCount, final int aExpectedValue )
  {
    this.originalValue = aOriginalValue;
    this.bitCount = aBitCount;
    this.expectedValue = aExpectedValue;
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
        // { original, bit count, expected }
            { 0, 7, 0 }, //
            { 1, 7, 64 }, //
            { 2, 7, 32 }, //
            { 4, 7, 16 }, //
            { 8, 7, 8 }, //
            { 16, 7, 4 }, //
            { 32, 7, 2 }, //
            { 64, 7, 1 }, //
            //
            { 1, 8, 128 }, //
            { 2, 8, 64 }, //
            { 4, 8, 32 }, //
            { 8, 8, 16 }, //
            { 16, 8, 8 }, //
            { 32, 8, 4 }, //
            { 64, 8, 2 }, //
            { 128, 8, 1 }, //
            { 0xAA, 8, 0x55 }, //
            { 0x55, 8, 0xAA }, //
            //
            { 1, 9, 256 }, //
            { 2, 9, 128 }, //
            { 4, 9, 64 }, //
            { 8, 9, 32 }, //
            { 16, 9, 16 }, //
            { 32, 9, 8 }, //
            { 64, 9, 4 }, //
            { 128, 9, 2 }, //
            { 256, 9, 1 }, //
            //
            { 1, 16, 0x8000 }, //
            { 0x8000, 16, 1 }, //
            //
            { 1, 32, 0x80000000 }, //
            { 0x80000000, 32, 1 }, //
        } );
  }

  /**
   * Tests whether the reversing of bits works correctly.
   */
  @Test
  public void testReverseBitsOk()
  {
    assertEquals( this.expectedValue, reverseBits( this.originalValue, this.bitCount ) );
  }
}
