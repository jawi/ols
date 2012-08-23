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
package nl.lxtreme.ols.tool.serialdebug.terminal.impl;


import static org.junit.Assert.*;

import nl.lxtreme.ols.tool.serialdebug.terminal.impl.TerminalImpl.*;

import org.junit.*;


/**
 * Test cases for {@link TerminalImpl}.
 */
public class TerminalImplTest
{
  // METHODS

  @Test
  public void testClearEnitreScreenOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 2, 1 );

    term.clearScreen( 2 );
    assertEquals( "              ", getTermText( term ) );

    assertEquals( 0, term.getCursor().getX() );
    assertEquals( 0, term.getCursor().getY() );
  }

  @Test
  public void testClearFirstLineOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 0, 0 );

    term.clearLine( 2 );
    assertEquals( "     123451234", getTermText( term ) );

    assertEquals( 0, term.getCursor().getX() );
    assertEquals( 0, term.getCursor().getY() );
  }

  @Test
  public void testClearLinePartiallyOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 1 );

    term.clearLine( 1 );
    assertEquals( "12345 23451234", getTermText( term ) );

    assertEquals( 1, term.getCursor().getX() );
    assertEquals( 1, term.getCursor().getY() );

    term.clearLine( 0 );
    assertEquals( "12345     1234", getTermText( term ) );

    assertEquals( 1, term.getCursor().getX() );
    assertEquals( 1, term.getCursor().getY() );
  }

  @Test
  public void testClearScreenAboveOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 2, 1 );

    term.clearScreen( 1 );
    assertEquals( "       3451234", getTermText( term ) );

    assertEquals( 2, term.getCursor().getX() );
    assertEquals( 1, term.getCursor().getY() );
  }

  @Test
  public void testClearScreenBelowOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 2, 1 );

    term.clearScreen( 0 );
    assertEquals( "1234512       ", getTermText( term ) );

    assertEquals( 2, term.getCursor().getX() );
    assertEquals( 1, term.getCursor().getY() );
  }

  @Test
  public void testClearSecondLineOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 2, 1 );

    term.clearLine( 2 );
    assertEquals( "12345     1234", getTermText( term ) );

    assertEquals( 2, term.getCursor().getX() );
    assertEquals( 1, term.getCursor().getY() );
  }

  @Test
  public void testClearThirdLineOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.clearLine( 2 );
    assertEquals( "1234512345    ", getTermText( term ) );

    assertEquals( 1, term.getCursor().getX() );
    assertEquals( 2, term.getCursor().getY() );
  }

  @Test
  public void testScrollDownOneLineOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.scrollDown( 1 );
    assertEquals( "     123451234", getTermText( term ) );
  }

  @Test
  public void testScrollDownThreeLinesOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.scrollDown( 3 );
    assertEquals( "              ", getTermText( term ) );
  }

  @Test
  public void testScrollDownTwoLinesOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.scrollDown( 2 );
    assertEquals( "          1234", getTermText( term ) );
  }

  @Test
  public void testScrollUpOneLineOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.scrollUp( 1 );
    assertEquals( "1234512345    ", getTermText( term ) );
  }

  @Test
  public void testScrollUpThreeLinesOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.scrollUp( 3 );
    assertEquals( "              ", getTermText( term ) );
  }

  @Test
  public void testScrollUpTwoLinesOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 1, 2 );

    term.scrollUp( 2 );
    assertEquals( "12345         ", getTermText( term ) );
  }

  @Test
  public void testWriteBackspacesAtLastPositionDoesNotScrollUpOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 0, 0 );
    term.writeText( "abcdefghijklmn" );
    assertEquals( "abcdefghijklmn", getTermText( term ) );

    term.writeText( "\b\b" );
    assertEquals( "abcdefghijkl  ", getTermText( term ) );

    term.writeText( "op" );
    assertEquals( "abcdefghijklop", getTermText( term ) );
  }

  @Test
  public void testWriteTextAtLastPositionScrollUpOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.moveCursorAbsolute( 0, 0 );
    term.writeText( "abcdefghijklmn" );
    assertEquals( "abcdefghijklmn", getTermText( term ) );

    term.writeText( "op" );
    assertEquals( "fghijklmnop   ", getTermText( term ) );
  }

  @Test
  public void testWriteTextHandlesNewlinesOk()
  {
    TerminalImpl term = createTerminalImpl();
    term.clearScreen( 2 );
    term.writeText( "ab\ncd\r\nef" );
    assertEquals( "ab     cd ef  ", getTermText( term ) );
  }

  /**
   * @return a new {@link TerminalImpl} instance, never <code>null</code>.
   */
  private TerminalImpl createTerminalImpl()
  {
    TerminalImpl term = new TerminalImpl( 5, 3 );
    term.writeText( "12345123451234" );
    return term;
  }

  /**
   * @param aTerm
   * @return
   */
  private String getTermText( final TerminalImpl aTerm )
  {
    StringBuilder sb = new StringBuilder();
    for ( int idx = aTerm.getFirstAbsoluteIndex(); idx < aTerm.getLastAbsoluteIndex(); idx++ )
    {
      TextCell cell = aTerm.getCellAt( idx );
      sb.append( cell == null ? ' ' : cell.getText() );
    }
    return sb.toString();
  }
}
