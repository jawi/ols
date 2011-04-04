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

import java.nio.*;

import nl.lxtreme.ols.util.NumberUtils.BitOrder;
import nl.lxtreme.ols.util.NumberUtils.UnitDefinition;

import org.junit.*;


/**
 * @author jawi
 */
public class NumberUtilsTest
{
  // METHODS

  /**
   * 
   */
  @Test
  public void testConvertBitOrderOk()
  {
    assertEquals( 0, reverseBits( 0, 8 ) );
    assertEquals( 128, reverseBits( 1, 8 ) );
    assertEquals( 32768, reverseBits( 1, 16 ) );
    assertEquals( Integer.MIN_VALUE, reverseBits( 1, 32 ) );

    assertEquals( 256, convertBitOrder( 1, 9, BitOrder.LSB_FIRST ) );
    assertEquals( 1, convertBitOrder( 256, 9, BitOrder.LSB_FIRST ) );
    assertEquals( 1, convertBitOrder( convertBitOrder( 1, 9, BitOrder.LSB_FIRST ), 9, BitOrder.LSB_FIRST ) );

    assertEquals( 1, convertBitOrder( 1, 9, BitOrder.MSB_FIRST ) );
    assertEquals( 128, convertBitOrder( 128, 9, BitOrder.MSB_FIRST ) );
    assertEquals( 1, convertBitOrder( convertBitOrder( 1, 9, BitOrder.MSB_FIRST ), 9, BitOrder.MSB_FIRST ) );

    assertEquals( 256, convertBitOrder( convertBitOrder( 1, 9, BitOrder.LSB_FIRST ), 9, BitOrder.MSB_FIRST ) );
  }

  /**
   * 
   */
  @Test
  public void testConvertByteOrderOk()
  {
    assertEquals( 0xBEBC200, convertByteOrder( 0x00C2EB0B, 32, ByteOrder.LITTLE_ENDIAN ) );
    assertEquals( 0x6000, convertByteOrder( 0x00600000, 32, ByteOrder.LITTLE_ENDIAN ) );
  }

  /**
   * 
   */
  @Test
  public void testGetBitIndexOk()
  {
    assertEquals( 8, getBitIndex( 256 ) );
    assertEquals( 7, getBitIndex( 128 ) );
    assertEquals( 6, getBitIndex( 64 ) );
    assertEquals( 5, getBitIndex( 32 ) );
    assertEquals( 4, getBitIndex( 16 ) );
    assertEquals( 3, getBitIndex( 8 ) );
    assertEquals( 2, getBitIndex( 4 ) );
    assertEquals( 1, getBitIndex( 2 ) );
    assertEquals( 0, getBitIndex( 1 ) );

    // special cases...
    assertEquals( 1, getBitIndex( 3 ) );
    assertEquals( 2, getBitIndex( 5 ) );
    assertEquals( 2, getBitIndex( 6 ) );
    assertEquals( 2, getBitIndex( 7 ) );
    assertEquals( 3, getBitIndex( 9 ) );
  }

  /**
   * 
   */
  @Test
  public void testGetPercentageWithBoundsOk()
  {
    assertEquals( 0, getPercentage( -1, 1, 11 ) );
    assertEquals( 0, getPercentage( 0, 1, 11 ) );
    assertEquals( 0, getPercentage( 1, 1, 11 ) );
    assertEquals( 10, getPercentage( 2, 1, 11 ) );
    assertEquals( 20, getPercentage( 3, 1, 11 ) );
    assertEquals( 30, getPercentage( 4, 1, 11 ) );
    assertEquals( 40, getPercentage( 5, 1, 11 ) );
    assertEquals( 50, getPercentage( 6, 1, 11 ) );
    assertEquals( 60, getPercentage( 7, 1, 11 ) );
    assertEquals( 70, getPercentage( 8, 1, 11 ) );
    assertEquals( 80, getPercentage( 9, 1, 11 ) );
    assertEquals( 90, getPercentage( 10, 1, 11 ) );
    assertEquals( 100, getPercentage( 11, 1, 11 ) );
    assertEquals( 100, getPercentage( 12, 1, 11 ) );
  }

  /**
   * 
   */
  @Test
  public void testGetPercentageWithRangeOk()
  {
    assertEquals( 0, getPercentage( -1, 10 ) );
    assertEquals( 0, getPercentage( 0, 10 ) );
    assertEquals( 10, getPercentage( 1, 10 ) );
    assertEquals( 20, getPercentage( 2, 10 ) );
    assertEquals( 30, getPercentage( 3, 10 ) );
    assertEquals( 40, getPercentage( 4, 10 ) );
    assertEquals( 50, getPercentage( 5, 10 ) );
    assertEquals( 60, getPercentage( 6, 10 ) );
    assertEquals( 70, getPercentage( 7, 10 ) );
    assertEquals( 80, getPercentage( 8, 10 ) );
    assertEquals( 90, getPercentage( 9, 10 ) );
    assertEquals( 100, getPercentage( 10, 10 ) );
    assertEquals( 100, getPercentage( 11, 10 ) );
  }

  @Test
  public void testPackBytes()
  {
    assertEquals( 0x1234, packBytes( 0x00001234 ) );
    assertEquals( 0x1234, packBytes( 0x12003400 ) );
    assertEquals( 0x1234, packBytes( 0x00120034 ) );
    assertEquals( 0x1234, packBytes( 0x12003400 ) );

    assertEquals( 0x12, packBytes( 0x00120000 ) );
    assertEquals( 0x123456, packBytes( 0x12003456 ) );
    assertEquals( 0x12345678, packBytes( 0x12345678 ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntBinaryUnitOk()
  {
    assertEquals( 4096, smartParseInt( "4k" ) );
    assertEquals( 4096, smartParseInt( "4K" ) );
    assertEquals( 4096 * 1024, smartParseInt( "4M" ) );
    assertEquals( 4096 * 1024, smartParseInt( "4  M" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntFail()
  {
    assertEquals( 0, NumberUtils.smartParseInt( "test" ) );
    assertEquals( 0, NumberUtils.smartParseInt( "" ) );
    assertEquals( 1, NumberUtils.smartParseInt( "", 1 ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntOk()
  {
    assertEquals( -1, NumberUtils.smartParseInt( "-1" ) );
    assertEquals( 1, NumberUtils.smartParseInt( "1" ) );
    assertEquals( 2, NumberUtils.smartParseInt( "2 " ) );
    assertEquals( 3, NumberUtils.smartParseInt( "3 4" ) );
    assertEquals( 4, NumberUtils.smartParseInt( "4,5" ) );
    assertEquals( 4, NumberUtils.smartParseInt( "4Hz" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4k" ) );
    assertEquals( 4096, NumberUtils.smartParseInt( "4K" ) );
    assertEquals( 4194304, NumberUtils.smartParseInt( "4M" ) );
    assertEquals( 4194304, NumberUtils.smartParseInt( "4  M" ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntSIUnitOk()
  {
    assertEquals( 4000, NumberUtils.smartParseInt( "4k", UnitDefinition.SI ) );
    assertEquals( 4000, NumberUtils.smartParseInt( "4K", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4M", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4  M", UnitDefinition.SI ) );
    assertEquals( 4000000, NumberUtils.smartParseInt( "4  MB", UnitDefinition.SI ) );
  }

  /**
   * 
   */
  @Test
  public void testSmartParseIntWithNullArgument()
  {
    assertEquals( 0, NumberUtils.smartParseInt( null ) );
    assertEquals( -1, NumberUtils.smartParseInt( null, -1 ) );
  }
}
