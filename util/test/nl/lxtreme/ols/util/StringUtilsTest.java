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
package nl.lxtreme.ols.util;


import static org.junit.Assert.*;

import org.junit.*;


/**
 * @author jawi
 */
public class StringUtilsTest
{

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.StringUtils#tokenize(java.lang.String, java.lang.String)}
   * .
   */
  @Test
  public void testTokenize()
  {
    String[] tokens;

    tokens = StringUtils.tokenize( "hello world", " " );
    assertArrayEquals( new String[] { "hello", "world" }, tokens );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.StringUtils#tokenizeQuotedStrings(java.lang.String, java.lang.String)}
   * .
   */
  @Test
  public void testTokenizeQuotedStrings()
  {
    String[] tokens;

    tokens = StringUtils.tokenizeQuotedStrings( "hello world", " " );
    assertArrayEquals( new String[] { "hello", "world" }, tokens );

    tokens = StringUtils.tokenizeQuotedStrings( "\"hello world\"", " " );
    assertArrayEquals( new String[] { "hello world" }, tokens );

    tokens = StringUtils.tokenizeQuotedStrings( "\"hello world\", \"foo\", 'bar'", ", " );
    assertArrayEquals( new String[] { "hello world", "foo", "'bar'" }, tokens );

    tokens = StringUtils.tokenizeQuotedStrings( "\"hello, world\", \"foo   bar\", 'bar'", ", " );
    assertArrayEquals( new String[] { "hello, world", "foo   bar", "'bar'" }, tokens );

    tokens = StringUtils.tokenizeQuotedStrings( "\"hello world of darkness\", \"foo   bar\", 'bar'", ", " );
    assertArrayEquals( new String[] { "hello world of darkness", "foo   bar", "'bar'" }, tokens );

    tokens = StringUtils.tokenizeQuotedStrings( "\"hello , darkness\", \"foo \t bar\", 'bar'", ", " );
    assertArrayEquals( new String[] { "hello , darkness", "foo \t bar", "'bar'" }, tokens );
  }

  /**
   * Test method for
   * {@link nl.lxtreme.ols.util.StringUtils#unquote(java.lang.String)}.
   */
  @Test
  public void testUnquoteString()
  {
    assertEquals( "hello world", StringUtils.unquote( "\"hello world\"" ) );
    assertEquals( "\"hello world", StringUtils.unquote( "\"hello world" ) );
    assertEquals( "hello world\"", StringUtils.unquote( "hello world\"" ) );

    assertEquals( "hello world", StringUtils.unquote( "'hello world'", '\'' ) );
    assertEquals( "'hello world", StringUtils.unquote( "'hello world", '\'' ) );
    assertEquals( "hello world'", StringUtils.unquote( "hello world'", '\'' ) );
  }
}
