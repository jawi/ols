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

import org.junit.*;


/**
 * Test cases for {@link StringInterpreter}.
 */
public class StringInterpreterTest
{
  // VARIABLES

  private StringInterpreter interpreter;

  // METHODS

  /**
   * Set up of this test case.
   */
  @Before
  public void setUp()
  {
    this.interpreter = new StringInterpreter();

    this.interpreter.setAppendNewLine( false );
  }

  /**
   * Tests that interpreting an empty or null string causes only a newline to be
   * emitted if this setting is enabled.
   */
  @Test
  public void testInterpretEmtpyStringWithAppendNewLine() throws Exception
  {
    this.interpreter.setAppendNewLine( true );

    assertInterpretation( "", '\n' );
    assertInterpretation( null, '\n' );
  }

  /**
   * Tests that interpreting an empty or null string causes only a newline to be
   * emitted if this setting is disable.
   */
  @Test
  public void testInterpretEmtpyStringWithoutAppendNewLine() throws Exception
  {
    assertInterpretation( "" );
    assertInterpretation( null );
  }

  /**
   * Tests that interpreting '\\' works.
   */
  @Test
  public void testInterpretEscapedBackslashCharOk() throws Exception
  {
    assertInterpretation( "\\\\", '\\' );
  }

  /**
   * Tests that interpreting hex digits as '\n' works.
   */
  @Test
  public void testInterpretEscapedCharsOk() throws Exception
  {
    assertInterpretation( "\\a\\r\\n", '\\', 'a', '\r', '\n' );
  }

  /**
   * Tests that interpreting hex digits as '\00' works.
   */
  @Test
  public void testInterpretEscapedDigitsOk() throws Exception
  {
    assertInterpretation( "\\00\\01\\02\\03\\04", 0, 1, 2, 3, 4 );
    assertInterpretation( "\\5\\6", 5, 6 );
    assertInterpretation( "\\1\\01\\001\\0001", 1, 1, 1, 0, '1' );
    assertInterpretation( "\\255", 255 );
    assertInterpretation( "\\\\255", '\\', '2', '5', '5' );
    assertInterpretation( "\\256", '\\', '2', '5', '6' );
    assertInterpretation( "\\\\256", '\\', '2', '5', '6' );
  }

  /**
   * Tests that interpreting '$$' works.
   */
  @Test
  public void testInterpretEscapedStringCharOk() throws Exception
  {
    assertInterpretation( "$$", '$' );
  }

  /**
   * Tests that interpreting hex digits as '$00' works.
   */
  @Test
  public void testInterpretHexDigitsOk() throws Exception
  {
    assertInterpretation( "$00$01$02$03$04", 0, 1, 2, 3, 4 );
    assertInterpretation( "$1$01$001$0001", 1, 1, 1, 0, '1' );
    assertInterpretation( "$255", 255 );
    assertInterpretation( "$$255", '$', '2', '5', '5' );
    assertInterpretation( "$256", '$', '2', '5', '6' );
    assertInterpretation( "$$256", '$', '2', '5', '6' );
  }

  /**
   * Tests the handling for non-escaped characters.
   */
  @Test
  public void testInterpretNonEscapedTextOk() throws Exception
  {
    String text = "The quick brown fox jumped over the lazy dog!";
    int[] values = new int[text.length()];
    for (int i = 0; i < text.length(); i++) {
      values[i] = text.charAt( i );
    }
    
    assertInterpretation( text, values );
  }

  /**
   * Tests that an unescaped '\' at the end of a string gets outputted
   * literally.
   */
  @Test
  public void testInterpretUnescapedBackslashAtEndOk() throws Exception
  {
    assertInterpretation( "\\", '\\' );
    assertInterpretation( "\\\\", '\\' );
    assertInterpretation( "\\\\\\", '\\', '\\' );
    assertInterpretation( "test\\", 't', 'e', 's', 't', '\\' );
  }

  /**
   * Tests that an unescaped '$' at the end of a string gets outputted
   * literally.
   */
  @Test
  public void testInterpretUnescapedStringCharAtEndOk() throws Exception
  {
    assertInterpretation( "$", '$' );
    assertInterpretation( "$$", '$' );
    assertInterpretation( "$$$", '$', '$' );
    assertInterpretation( "test$", 't', 'e', 's', 't', '$' );
  }

  /**
   * Tests that given a unicode character, the result will be an UTF-8 encoded
   * byte value.
   */
  @Test
  public void testInterpretUnicodeOk() throws Exception
  {
    assertInterpretation( "\u1234", 0x1234 );
    assertInterpretation( "\u00e9", 0xe9 );
    assertInterpretation( "\u0027", 0x27 );
  }

  /**
   * @param aInput
   * @param aExpectedValues
   */
  private void assertInterpretation( final String aInput, final int... aExpectedValues )
  {
    String result = this.interpreter.interpret( aInput );
    for ( int i = 0; i < result.length(); i++ )
    {
      assertEquals( Integer.valueOf( aExpectedValues[i] ), Integer.valueOf( result.charAt( i ) ) );
    }
    assertEquals( aExpectedValues.length, result.length() );
  }
}
