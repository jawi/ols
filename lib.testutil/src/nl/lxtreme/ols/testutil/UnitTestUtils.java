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
 * Copyright (C) 2010-2011 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.testutil;


import junit.framework.*;


/**
 * Provides some additional test methods to JUnit.
 */
public final class UnitTestUtils
{
  // CONSTRUCTORS

  /**
   * Creates a new UnitTestUtils instance.
   */
  private UnitTestUtils()
  {
    // NO-op
  }

  // METHODS

  public static void assertArrayEquals( final int[] expected, final int[] input )
  {
    Assert.assertEquals( "Length mismatch!", expected.length, input.length );
    for ( int i = 0; i < expected.length; i++ )
    {
      Assert.assertEquals( "Element @ " + i, expected[i], input[i] );
    }
  }

  public static void assertArrayEquals( final long[] expected, final long[] input )
  {
    Assert.assertEquals( "Length mismatch!", expected.length, input.length );
    for ( int i = 0; i < expected.length; i++ )
    {
      Assert.assertEquals( "Element @ " + i, expected[i], input[i] );
    }
  }

  public static void assertArrayEquals( final Object[] expected, final Object[] input )
  {
    Assert.assertEquals( "Length mismatch!", expected.length, input.length );
    for ( int i = 0; i < expected.length; i++ )
    {
      Assert.assertEquals( "Element @ " + i, expected[i], input[i] );
    }
  }

  /**
   * Asserts that a given value is within the given boundaries, that is,
   * <tt>aLowerBound &lt;= aValue &lt;= aUpperBound</tt>.
   * 
   * @param aLowerBound
   *          the lower boundary;
   * @param aUpperBound
   *          the upper boundary;
   * @param aValue
   *          the value to test.
   */
  public static void assertRange( final double aLowerBound, final double aUpperBound, final double aValue )
  {
    final String message = String.format( "Value (%f) is not in the expected range [%f..%f]", Double.valueOf( aValue ),
        Double.valueOf( aLowerBound ), Double.valueOf( aUpperBound ) );
    assertRange( message, aLowerBound, aUpperBound, aValue );
  }

  /**
   * Asserts that a given value is within the given boundaries, that is,
   * <tt>aLowerBound &lt;= aValue &lt;= aUpperBound</tt>.
   * 
   * @param aLowerBound
   *          the lower boundary;
   * @param aUpperBound
   *          the upper boundary;
   * @param aValue
   *          the value to test.
   */
  public static void assertRange( final int aLowerBound, final int aUpperBound, final int aValue )
  {
    final String message = String.format( "Value (%f) is not in the expected range [%f..%f]", Double.valueOf( aValue ),
        Double.valueOf( aLowerBound ), Double.valueOf( aUpperBound ) );
    assertRange( message, aLowerBound, aUpperBound, aValue );
  }

  /**
   * Asserts that a given value is within the given boundaries, that is,
   * <tt>aLowerBound &lt;= aValue &lt;= aUpperBound</tt>.
   * 
   * @param aLowerBound
   *          the lower boundary;
   * @param aUpperBound
   *          the upper boundary;
   * @param aValue
   *          the value to test.
   */
  public static void assertRange( final long aLowerBound, final long aUpperBound, final long aValue )
  {
    final String message = String.format( "Value (%f) is not in the expected range [%f..%f]", Double.valueOf( aValue ),
        Double.valueOf( aLowerBound ), Double.valueOf( aUpperBound ) );
    assertRange( message, aLowerBound, aUpperBound, aValue );
  }

  /**
   * Asserts that a given value is within the given boundaries, that is,
   * <tt>aLowerBound &lt;= aValue &lt;= aUpperBound</tt>.
   * 
   * @param aMessage
   *          the message to print when the assertion fails;
   * @param aLowerBound
   *          the lower boundary;
   * @param aUpperBound
   *          the upper boundary;
   * @param aValue
   *          the value to test.
   */
  public static void assertRange( final String aMessage, final double aLowerBound, final double aUpperBound,
      final double aValue )
  {
    Assert.assertTrue( aMessage, aLowerBound <= aValue );
    Assert.assertTrue( aMessage, aValue <= aUpperBound );
  }

  /**
   * Asserts that a given value is within the given boundaries, that is,
   * <tt>aLowerBound &lt;= aValue &lt;= aUpperBound</tt>.
   * 
   * @param aMessage
   *          the message to print when the assertion fails;
   * @param aLowerBound
   *          the lower boundary;
   * @param aUpperBound
   *          the upper boundary;
   * @param aValue
   *          the value to test.
   */
  public static void assertRange( final String aMessage, final int aLowerBound, final int aUpperBound, final int aValue )
  {
    Assert.assertTrue( aMessage, aLowerBound <= aValue );
    Assert.assertTrue( aMessage, aValue <= aUpperBound );
  }

  /**
   * Asserts that a given value is within the given boundaries, that is,
   * <tt>aLowerBound &lt;= aValue &lt;= aUpperBound</tt>.
   * 
   * @param aMessage
   *          the message to print when the assertion fails;
   * @param aLowerBound
   *          the lower boundary;
   * @param aUpperBound
   *          the upper boundary;
   * @param aValue
   *          the value to test.
   */
  public static void assertRange( final String aMessage, final long aLowerBound, final long aUpperBound,
      final long aValue )
  {
    Assert.assertTrue( aMessage, aLowerBound <= aValue );
    Assert.assertTrue( aMessage, aValue <= aUpperBound );
  }
}
