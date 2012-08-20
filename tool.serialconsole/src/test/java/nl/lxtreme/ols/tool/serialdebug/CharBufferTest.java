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
package nl.lxtreme.ols.tool.serialdebug;


import static org.junit.Assert.*;

import java.util.*;

import org.junit.*;


/**
 * Test cases for {@link CharBuffer}.
 */
@SuppressWarnings( "boxing" )
public class CharBufferTest
{
  // METHODS

  /**
   * Tests that calling {@link CharBuffer#append(java.util.List)} with a
   * non-null value succeeds.
   */
  @Test
  public void testAppendNotNullOk()
  {
    CharBuffer charBuffer = new CharBuffer();
    charBuffer.append( Arrays.asList( 1, 2, 3 ) );

    for ( int i = 0; i < charBuffer.length(); i++ )
    {
      assertEquals( ( char )( i + 1 ), charBuffer.charAt( i ) );
    }
  }

  /**
   * Tests that calling {@link CharBuffer#append(java.util.List)} with a null
   * value fails.
   */
  @Test( expected = IllegalArgumentException.class )
  public void testAppendNullFail()
  {
    new CharBuffer().append( null );
  }

  /**
   * Tests that calling {@link CharBuffer#append(java.util.List)} with a
   * non-null value succeeds.
   */
  @Test
  public void testAppendWillResizeInternalArrayOk()
  {
    CharBuffer charBuffer = new CharBuffer();
    for ( int i = 0; i < 100; i++ )
    {
      charBuffer.append( Arrays.asList( i + 1 ) );
    }

    for ( int i = 0; i < charBuffer.length(); i++ )
    {
      assertEquals( ( char )( i + 1 ), charBuffer.charAt( i ) );
    }
  }

  /**
   * Tests that we cannot call {@link CharBuffer#charAt(int)} on an empty
   * buffer.
   */
  @Test( expected = IndexOutOfBoundsException.class )
  public void testCharAtEmptyBufferFail()
  {
    new CharBuffer().charAt( 0 );
  }

  /**
   * Tests that we cannot call {@link CharBuffer#charAt(int)} with index two
   * succeeds.
   */
  @Test
  public void testCharAtIndexTwoOk()
  {
    assertEquals( ( char )3, new CharBuffer( 1, 2, 3 ).charAt( 2 ) );
  }

  /**
   * Tests that we cannot call {@link CharBuffer#charAt(int)} with index zero
   * succeeds.
   */
  @Test
  public void testCharAtIndexZeroOk()
  {
    assertEquals( ( char )1, new CharBuffer( 1, 2, 3 ).charAt( 0 ) );
  }

  /**
   * Tests that we cannot call {@link CharBuffer#charAt(int)} with an index
   * beyond the end of the buffer fails.
   */
  @Test( expected = IndexOutOfBoundsException.class )
  public void testCharAtWithIndexBeyondLastPositionFail()
  {
    new CharBuffer( 1, 2, 3 ).charAt( 3 );
  }

  /**
   * Tests that we cannot call {@link CharBuffer#charAt(int)} with a negative
   * index fails.
   */
  @Test( expected = IndexOutOfBoundsException.class )
  public void testCharWithNegativeIndexFail()
  {
    new CharBuffer( 1, 2, 3 ).charAt( -1 );
  }

  /**
   * Tests that the length for an empty buffer is zero.
   */
  @Test
  public void testLengthEmptyBufferOk()
  {
    assertEquals( 0, new CharBuffer().length() );
  }

  /**
   * Tests that the length for a non-empty buffer is correct.
   */
  @Test
  public void testLengthOk()
  {
    assertEquals( 3, new CharBuffer( 1, 2, 3 ).length() );
  }

  /**
   * Tests that the length for buffer that is resized at least once is correct.
   */
  @Test
  public void testLengthWithInternalResizedArrayOk()
  {
    CharBuffer cb = new CharBuffer();
    for ( int i = 0; i < 100; i++ )
    {
      cb.append( Arrays.asList( i ) );
    }
    assertEquals( 100, cb.length() );
  }

  /**
   * Tests that calling {@link CharBuffer#removeUntil(int)} with a negative
   * position fails.
   */
  @Test( expected = IndexOutOfBoundsException.class )
  public void testRemoveUntilWithNegativePositionFails()
  {
    new CharBuffer( 1, 2, 3 ).removeUntil( -1 );
  }

  /**
   * Tests that calling {@link CharBuffer#removeUntil(int)} with a negative
   * position fails.
   */
  @Test( expected = IndexOutOfBoundsException.class )
  public void testRemoveUntilWithPositionBeyondLengthFails()
  {
    new CharBuffer( 1, 2, 3 ).removeUntil( 4 );
  }

  /**
   * Tests that calling {@link CharBuffer#removeUntil(int)} with a position of
   * two yields an empty charbuffer.
   */
  @Test
  public void testRemoveUntilWithPositionOneOk()
  {
    CharBuffer cb = new CharBuffer( 1, 2, 3 );
    cb.removeUntil( 1 );

    assertEquals( 2, cb.length() );
    for ( int i = 0; i < cb.length(); i++ )
    {
      assertEquals( ( char )( i + 2 ), cb.charAt( i ) );
    }
  }

  /**
   * Tests that calling {@link CharBuffer#removeUntil(int)} with a position of
   * two yields an empty charbuffer.
   */
  @Test
  public void testRemoveUntilWithPositionTwoOk()
  {
    CharBuffer cb = new CharBuffer( 1, 2, 3 );
    cb.removeUntil( 2 );

    assertEquals( 0, cb.length() );
  }

  /**
   * Tests that calling {@link CharBuffer#removeUntil(int)} with a position of
   * zero yields the same charbuffer.
   */
  @Test
  public void testRemoveUntilWithPositionZeroOk()
  {
    CharBuffer cb = new CharBuffer( 1, 2, 3 );
    cb.removeUntil( 0 );

    assertEquals( 3, cb.length() );
    for ( int i = 0; i < 3; i++ )
    {
      assertEquals( ( char )( i + 1 ), cb.charAt( i ) );
    }
  }

}
