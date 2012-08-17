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
  public void testInterpretEmtpyStringWithAppendNewLine()
  {
    this.interpreter.setAppendNewLine( true );

    assertArrayEquals( new byte[] { '\n' }, this.interpreter.interpret( "" ) );
    assertArrayEquals( new byte[] { '\n' }, this.interpreter.interpret( null ) );
  }

  /**
   * Tests that interpreting an empty or null string causes only a newline to be
   * emitted if this setting is disable.
   */
  @Test
  public void testInterpretEmtpyStringWithoutAppendNewLine()
  {
    assertArrayEquals( new byte[0], this.interpreter.interpret( "" ) );
    assertArrayEquals( new byte[0], this.interpreter.interpret( null ) );
  }

  /**
   * Tests that interpreting '\\' works.
   */
  @Test
  public void testInterpretEscapedBackslashCharOk()
  {
    assertEquals( "\\", interpret( "\\\\" ) );
  }

  /**
   * Tests that interpreting hex digits as '\n' works.
   */
  @Test
  public void testInterpretEscapedCharsOk()
  {
    assertEquals( "\\a\r\n", interpret( "\\a\\r\\n" ) );
  }

  /**
   * Tests that interpreting hex digits as '\00' works.
   */
  @Test
  public void testInterpretEscapedDigitsOk()
  {
    assertEquals( "\0\1\2\3\4\5\6", interpret( "\\00\\01\\02\\03\\04\\5\\6" ) );
    assertEquals( "\1\1\1\0001", interpret( "\\1\\01\\001\\0001" ) );
    assertEquals( "\u00ff", interpret( "\\255" ) );
    assertEquals( "\\255", interpret( "\\\\255" ) );
    assertEquals( "\\256", interpret( "\\256" ) );
    assertEquals( "\\256", interpret( "\\\\256" ) );
  }

  /**
   * Tests that interpreting '$$' works.
   */
  @Test
  public void testInterpretEscapedStringCharOk()
  {
    assertEquals( "$", interpret( "$$" ) );
  }

  /**
   * Tests that interpreting hex digits as '$00' works.
   */
  @Test
  public void testInterpretHexDigitsOk()
  {
    assertEquals( "\u0000\u0001\u0002\u0003\u0004", interpret( "$00$01$02$03$04" ) );
    assertEquals( "\u0001\u0001\u0001\u00001", interpret( "$1$01$001$0001" ) );
    assertEquals( "\u00ff", interpret( "$255" ) );
    assertEquals( "$255", interpret( "$$255" ) );
    assertEquals( "$256", interpret( "$256" ) );
    assertEquals( "$256", interpret( "$$256" ) );
  }

  /**
   * Tests the handling for non-escaped characters.
   */
  @Test
  public void testInterpretNonEscapedTextOk()
  {
    assertEquals( "The quick brown fox jumped over the lazy dog!",
        interpret( "The quick brown fox jumped over the lazy dog!" ) );
  }

  /**
   * Tests that an unescaped '\' at the end of a string gets outputted
   * literally.
   */
  @Test
  public void testInterpretUnescapedBackslashAtEndOk()
  {
    assertEquals( "\\", interpret( "\\" ) );
    assertEquals( "\\", interpret( "\\\\" ) );
    assertEquals( "\\\\", interpret( "\\\\\\" ) );
    assertEquals( "test\\", interpret( "test\\" ) );
  }

  /**
   * Tests that an unescaped '$' at the end of a string gets outputted
   * literally.
   */
  @Test
  public void testInterpretUnescapedStringCharAtEndOk()
  {
    assertEquals( "$", interpret( "$" ) );
    assertEquals( "$", interpret( "$$" ) );
    assertEquals( "$$", interpret( "$$$" ) );
    assertEquals( "test$", interpret( "test$" ) );
  }

  /**
   * @param input
   * @return
   */
  private String interpret( final String input )
  {
    return new String( this.interpreter.interpret( input ) );
  }
}
