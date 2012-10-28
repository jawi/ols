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
package nl.lxtreme.ols.tool.base;


import java.util.*;

import junit.framework.*;


/**
 * Tests the reversing of bits in a byte, word and so on.
 */
@SuppressWarnings( "boxing" )
public class NumberUtilsTest extends TestCase
{
  // METHODS

  /**
   * @return a collection of test data.
   */
  public static Collection<Integer[]> getTestData()
  {
    return Arrays.asList( new Integer[][] { //
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
  public void testReverseBitsOk()
  {
    for ( Integer[] params : getTestData() )
    {
      int expectedValue = params[0];
      int bitCount = params[1];
      int originalValue = params[2];

      assertEquals( expectedValue, NumberUtils.reverseBits( originalValue, bitCount ) );
    }
  }
}
